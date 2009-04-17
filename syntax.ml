open Parser_ctx
open Types
open Util
open ExtList
open ExtString
open Printf

exception Unsupported_type
exception Syntax_error

type name  = string

type ast_top    = Module of mod_props * parser_ctx 
 and block      = Block of blk_props * parser_ctx
 and definition =   FuncDef of func_props * parser_ctx
                  | ExternFunc of (name * beep_type) 
                  | TypeDef of (name * beep_type) * parser_ctx
                  | MacroDef of macro * parser_ctx
 and statement  = | StEmpty of parser_ctx
                  | StLocal of (name * beep_type) * parser_ctx
                  | StArg of (name * beep_type) * parser_ctx
                  | StAssign of expression * expression * parser_ctx
                  | StWhile of (expression * block) * parser_ctx
                  | StIf of if_props * parser_ctx
                  | StBranch of (expression * block) * parser_ctx
                  | StBranchElse of block * parser_ctx
                  | StCall of expression * parser_ctx
                  | StRet of expression * parser_ctx
                  | StBreak of parser_ctx
                  | StContinue of parser_ctx
                  | StEmit of Opcodes.opcode list
 and expression =   ELiteral of literal * parser_ctx 
                  | EIdent of name * parser_ctx
                  | ECall of (expression * expression list) * parser_ctx
                  | EAriphBin of operation * (expression * expression) * parser_ctx
                  | EAriphUn  of operation * expression * parser_ctx
                  | ECmp      of operation * (expression * expression) * parser_ctx
                  | EBoolBin  of operation * (expression * expression) * parser_ctx
                  | EBoolUn   of operation * expression * parser_ctx
                  | EListNil  of parser_ctx
                  | EList     of (expression * expression ) * parser_ctx
                  | EPair     of (expression * expression ) * parser_ctx
                  | ERecord   of (name * expression list) * parser_ctx
                  | ERecordFieldInit of (name * expression ) * parser_ctx
                  | ERecordField of rec_desc * rec_field
                  | ENothing
                  | EVoid of expression
                  | EQuot of name * parser_ctx
 and lvalue     = Named of name * parser_ctx
 and rec_desc   = Rec of expression
 and rec_field  = RecField of name * parser_ctx
 and operation  =   Plus | Minus | Mul | Div | Mod | BAnd | BOr | BXor | BShl | BShr | BInv
                  | Less | More | LessEq | MoreEq | Equal | Unequal | And | Or | Not
 and literal    = | LInt of int | LBool of bool | LString of string 
 and mod_props  = { mod_defs:definition list }
 and func_props = { func_name:name; func_type:beep_type; func_code:block }
 and blk_props  = { blk_code:statement list; }
 and if_props   = { if_then:statement; if_elif:statement list; if_else:statement }

 and macro      = MacroLiteral of (name * expression)

let with_args a = function Block(p,c) -> Block({blk_code = a @ p.blk_code},c)


let ast_module defs = Module({mod_defs = defs}, ctx())

let ast_func_def name args ret code  =
    let at = List.map (fun (_,t) -> t) args
    in let argz = List.map (fun (n,t) -> StArg((n,t),ctx())) args
    in FuncDef({func_name=name; func_type = TFun(at, ret); func_code=with_args argz code}, ctx()) 


let rec_field_offset tp s = match tp with
    | TRecord(fs) -> (
                      try fs |> List.sort ~cmp:(fun (a,_) (b,_) -> compare a b) 
                             |> List.mapi ( fun i (a,_) -> (a,i)) |> List.assoc s
                      with Not_found -> raise Syntax_error
                     )
    | _           -> assert false

let validate_type = function 
    |TRecord(fs)  -> let uni = fs |> List.unique ~cmp:(fun (a,_) (b,_) -> a = b )
                     in if List.length uni = List.length fs then TRecord(uni) else (raise Syntax_error)
    | _           -> raise Syntax_error

let ast_type_def name tp = TypeDef((name, validate_type tp), ctx())

let ast_block stmt =
    (* TODO: TAIL CALL CHECK *)
    Block({blk_code=List.rev stmt}, ctx())

let ast_stmt_empty = StEmpty(ctx())

let ast_stmt_local (name, tp) = StLocal((name, tp), ctx())

let ast_stmt_assign expr1 expr2 = StAssign(expr1, expr2, ctx())

let ast_stmt_while expr block = StWhile((expr, block), ctx())

let ast_stmt_if if_ else_if else_ = 
    StIf({ if_then = StBranch(if_, ctx());
           if_elif = List.map ( fun x -> StBranch(x, ctx()) ) else_if;
           if_else = StBranchElse(else_, ctx());
         }, ctx())

let ast_stmt_call expr = StCall(expr, ctx())

let ast_stmt_ret expr = StRet(expr, ctx())

let ast_stmt_break = StBreak (ctx())

let ast_stmt_continue = StContinue (ctx())

let ast_expr_call e args = ECall((e, args), ctx()) 

let ast_expr_lit x = ELiteral(x, ctx()) 

let ast_expr_ariph_bin op left right = EAriphBin( op, (left, right), ctx() )

let ast_expr_ariph_un  op operand    = EAriphUn( op, operand, ctx() )

let ast_expr_cmp op left right = ECmp(op, (left, right), ctx())

let ast_expr_bool_bin op left right = EBoolBin( op, (left, right), ctx() )

let ast_expr_bool_un  op operand    = EBoolUn( op, operand, ctx() )

let ast_expr_ident name = EIdent(name, ctx())

let ast_expr_pair a b = EPair((a,b), ctx())

let ast_expr_list_nil = EListNil(ctx())

let ast_expr_list h t = EList((h,t), ctx())

let ast_expr_rec_ctor name fields = ERecord((name, fields), ctx()) 

let ast_expr_rec_field_init (name,expr) = ERecordFieldInit((name,expr),ctx())

let ast_expr_rec_field (e,f) = ERecordField(Rec(e), RecField(f, ctx()))

let ast_var_x v = v

let unvoid tl = List.fold_left (fun acc x -> match x with TVoid -> acc | _ -> acc @ [x]) [] tl

let ast_macro_extern (name, tp) ast = 
(*     let _ = printf "MACRO EXTERN: %s -> %s\n" name (str_of_tp tp) in *)
    (function Module({mod_defs=defs},c) -> Module({mod_defs=ExternFunc((name,tp)) :: defs },c) ) ast

let ast_macro_literal (name,expr) = 
    let _ = printf "MACRO LITERAL: %s\n" name in
    (function Module({mod_defs=defs},c) -> Module({mod_defs=MacroDef(MacroLiteral(name,expr),ctx()) :: defs },c) )

let ast_quotation name = EQuot(name, ctx())

let mod_defs = function Module({mod_defs=defs},_) -> defs

let mod_funcs x = mod_defs x |> List.filter (function FuncDef _ -> true | _ -> false)

let mod_funcs_ext x = mod_defs x |> List.filter (function ExternFunc _ -> true | _ -> false)

let mod_typedefs x = mod_defs x |> List.filter (function TypeDef _ -> true | _ -> false )

let mod_macros x = mod_defs x |> List.filter (function MacroDef _ -> true | _ -> false )

let func_props = function FuncDef(p,c) -> p | _ -> assert false

let block_props = function Block(p,c) -> p

let code_of b = (block_props b).blk_code

(* let code_of_branch = function Branch((_,bl),_) -> code_of bl *)

let iter_defs f  = function Module({mod_defs=defs},_) -> defs |> List.iter f

let context_of_stmt = function
  | StEmpty(c)
  | StLocal(_,c)
  | StArg(_,c)
  | StAssign(_,_,c)
  | StWhile(_,c)
  | StIf(_, c) 
  | StBranch(_,c)
  | StBranchElse(_,c)
  | StCall(_,c)
  | StRet(_,c)
  | StBreak(c)
  | StContinue(c) -> c
  | StEmit _        -> assert false

