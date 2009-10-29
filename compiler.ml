module Compiler =  
struct
    type compiler_settings = { out_path:string; entry_point:string }

    open ExtList
    open ExtHashtbl
    open ExtString
    open Enum
    open Std
    open Util
    open Printf
    open Format

    open Syntax
    open Types
    open Beepvm_builtins
    open Opcodes
    open Code
    open Code_optimizer
    open Stubs

    exception Not_resolved  of name
    exception Undefined_type of name
    exception Undefined_fun_call
    exception Bad_arg_num
    exception Invalid_operation_type of beep_type
    exception No_field of beep_type * name
    exception Not_func_apply
    exception Type_error of string
    
    type compiler_err_type = SyntaxError | NameError of name | TypeError of string | ArgNumError
                             | NameHideError of string * string | OtherError of string
    type compiler_error_ctx = { 
                                comp_err_fname: string option;
                                comp_err_lnum: int 
                              }

    exception CompilerError of compiler_err_type * compiler_error_ctx

    let err_context ctx = 
        let ln = ctx.Parser_ctx.pos.Lexing.pos_lnum
        in let fn = ctx.Parser_ctx.pos.Lexing.pos_fname
        in {comp_err_fname=Some(fn);comp_err_lnum=ln}

    let raise_compiler_error_with x ste =
        let c = err_context ste
        in match x with
        | Type_error(s)   -> raise (CompilerError(TypeError(s), c))
        | Not_resolved(s) -> raise (CompilerError(NameError(s), c))
        | Bad_arg_num     -> raise (CompilerError(ArgNumError, c))
        | x               -> raise x

    let raise_compiler_error x stmt =
        raise_compiler_error_with x (context_of_stmt stmt)

    let raise_parse_error lex  =
        let _ = printf "parse error\n" in
        let ln = lex.Lexing.lex_curr_p.Lexing.pos_lnum
        in let fn = lex.Lexing.lex_curr_p.Lexing.pos_fname
        in raise (CompilerError(SyntaxError, {comp_err_fname=Some(fn); comp_err_lnum=ln}))

    let raise_type_error  = function
        | Circular           -> raise (Type_error("Circular constraint"))
        | Unsolvable(t1,t2)  -> raise (Type_error(sprintf "%s <-> %s" (str_of_tp t1) (str_of_tp t2)))
        | Unsupported(t1)    -> raise (Type_error(sprintf "Unsupported type for operation %s" (str_of_tp t1)))

    let check_name name ctx f =
        try (f ()) with Not_found -> raise_compiler_error_with (Not_resolved(name)) ctx

    let check_args ctx f =
        try (f ()) with List.Different_list_size(n) -> raise_compiler_error_with Bad_arg_num ctx

    let map_funcs f ast = mod_funcs ast |> f

    let runpack l = List.map (fun (_,e) -> e) l 

    let dot s = "." ^ s

    let fold_funcs f init ast =
        let folder_fun st fx = f st fx
        in ast |> mod_funcs |> List.fold_left folder_fun init

    let fold_defs f init ast =
        let folder_def st fx = f st fx
        in ast |> mod_defs |> List.fold_left folder_def init

    let rec fold_code merge f_code init code =
            let folder_code_ a b =
                let fc = f_code a b
                in let st1 = a
                in let st2 = match b with
                | StRet _ | StCall _ | StAssign _ 
                | StBreak _ | StContinue _
                | StArg _ | StLocal _ | StEmpty _ | StEmit _ -> fold_code merge f_code fc []
                | StBranch((_, Block({blk_code=code},_)), _) -> fold_code merge f_code fc code
                | StBranchElse(Block({blk_code=code},_), _)  -> fold_code merge f_code fc code
                | StWhile((_, Block({blk_code=code},_)), _)  -> fold_code merge f_code fc code
                | StIf(br, c)                                -> fold_if merge f_code fc br
                in merge st1 st2
                in let folder_code a b = try folder_code_ a b with x -> raise_compiler_error x b
                in List.fold_left folder_code init code
    and fold_if merge f_code init if_p =
        match if_p with
        {if_then=b1;if_elif=b2;if_else=b3} ->
            let c1    = fold_code merge f_code init [b1]
            in let c2 = fold_code merge f_code c1 b2
            in let c3 = fold_code merge f_code c2 [b3]
            in c3

    let rec fold_expresion merge f_exp init (exp:expression list) =
        let folder_expr acc b = match b with
        | ENothing
        | ELiteral  _
        | EListNil  _
        | EIdent    _              -> f_exp acc b
        | EVoid(ECall((e,a),_))
        | ECall((e,a),_)           -> let s1    = acc 
                                      in let s2 = fold_expresion merge f_exp s1 a
                                      in let s3 = fold_expresion merge f_exp s2 (e::[])
                                      in f_exp s3 b

        | EVoid(x)                 -> assert false

        | ERecord((n,ef),_)        -> let s1 = f_exp acc b
                                      in fold_expresion merge f_exp s1 ef

        | ERecordFieldInit((n,e),c)        -> f_exp (fold_expresion merge f_exp acc [e]) b
        | ERecordField(Rec(e),RecField(_)) -> f_exp (fold_expresion merge f_exp acc [e]) b

        | EBoolUn(op, e, _)
        | EAriphUn(op, e,_)        -> let s1 = (fold_expresion merge f_exp acc [e])
                                      in (f_exp s1 b)

        | EPair( (e1, e2), _ )
        | EList( (e1, e2), _ )
        | ECmp(_, (e1,e2), _ )
        | EBoolBin(_, (e1,e2), _)
        | EAriphBin(_, (e1,e2),_) ->  f_exp (fold_expresion merge f_exp acc [e1;e2]) b
        | EQuot _                 -> assert false
 
        in List.fold_left folder_expr init exp

    let id_of c = c.Parser_ctx.uid

    type var_t = { var_name:name; var_type:beep_type; }
    
    let var (n,t)  = { var_name=n; var_type=t; }

    type lt = { lt_vars:(name * var_t) list list; }

    type cctx = { c_resolv: ( (name * int), var_t) Hashtbl.t ;
                  c_nm_cache: ( (name * int), beep_type ) Hashtbl.t ;
                  c_glob: (name * var_t) list;
                  c_ast:ast_top;
                  c_unify: (beep_type*beep_type) list -> (beep_type*beep_type) list;
                  c_constr: (beep_type*beep_type) list }

    let globals ast =
        let mk x = match x with TypeDef((name,tp),_)  ->  ( TTypename(name), tp)
                                | _ -> assert false
        in let typedefs = mod_typedefs ast
        in let tds = typedefs |> List.map mk 
        in let repl = with_atom_type (fun x -> try List.assoc x tds with Not_found -> x)

        in let add_fields n x = 
            match x with TRecord(fields) -> List.map (fun (n,t) -> (dot n, var (n, TRecField((TRecord(fields),n))))  ) fields
                         | _             -> assert false

        in let folder_globs a b = match b with
            | FuncDef _        -> let fp = func_props b
                                  in (fp.func_name, var (fp.func_name, repl fp.func_type)) :: a
            | ExternFunc((n,t),_) -> (n, var (n, repl t) ) :: a
            | TypeDef((n,TRecord(t)),_)  ->  let trec = repl (TRecord(t))
                                             in (n, var (n, TTypedef(trec) )) :: add_fields n (TRecord(t)) @ a 
            | TypeDef((n,t),_)  -> (n, var (n, TTypedef(repl t) )) :: a
            | MacroDef _        -> a
        in let dict = fold_defs folder_globs [] ast (* TODO: CHECK UNIQUE *)
(*         in let _ = List.iter ( fun (n,{var_type=t}) -> printf "DICT: %s %s\n" n (str_of_tp t)) dict *)
        in dict 

    let names_of e =
        let merge a rs = a @ rs
        in let rec folder_names s = function
        | EIdent(name, c)         -> (name, id_of c) :: s
        | ERecord((name, l), c)   -> (name, id_of c) :: s
        | ERecordFieldInit((name,e),c) -> (dot name, id_of c) :: s
        | ERecordField(Rec(e),RecField(field,c2)) -> (dot field, id_of c2) :: s
        | _                       -> s
        in fold_expresion merge folder_names [] e


    let rec unwrap_type t ast = match t with
        | TTypedef(t)  -> t
        | TTypename(s) -> find_type s ast
        | x            -> x
    and find_type name ast =
        let pred  = function TypeDef((s,_),_)  ->  if name = s then true else false
                            | _                ->  false
        in try match (mod_typedefs ast |> List.find pred) with
               | TypeDef((_,tp),_)   -> unwrap_type tp ast
               | _                   -> raise (Undefined_type name)
           with Not_found -> raise (Undefined_type name) 

    (*  datavalues ast ctx -> [string] 
     *  find here strings, atoms, may be constants and so on. 
     *  only strings are supported on the moment
     *)
    let datavalues ast ctx = 
        let merge a b = a @ b |> List.unique
        in let fe acc = function
        | ELiteral(LString(s), _) -> s :: acc
        | _                       -> acc
        in let fc acc code = 
        match code with
        | StArg _           | StLocal _ 
        | StEmpty _         | StBreak _ 
        | StContinue _      | StIf _  
        | StEmit _
        | StBranchElse _                        -> acc
        | StWhile((e,_),_)   
        | StBranch((e,_),_) | StCall(e,_)
        | StRet(e,_)                            -> fold_expresion merge fe acc [e]
        | StAssign(e1,e2,_)                     -> fold_expresion merge fe acc [e1;e2]     
        in let ff (st:string list) fn =
            let fp = func_props fn
            in fold_code merge fc st (block_props fp.func_code).blk_code 
        in fold_funcs ff [] ast

    let lookup_table ast initial = 
        let init = { lt_vars=[initial]; }
        in let add_var (n,t) tbl = { lt_vars = with_head (fun x -> (n, var (n,t)) :: x) tbl.lt_vars ; }
        in let add_scope tbl = { lt_vars = ([]) :: tbl.lt_vars }
        in let ty t = unwrap_type t ast
        in let resolved = Enum.empty()
        in let resolve_name tbl (name, id) = 
            let flat = List.fold_left ( fun a b -> a @ b ) [] tbl.lt_vars
            in let var = try List.assoc name flat with Not_found -> raise (Not_resolved(name))
            in let _ = Enum.push resolved ((name, id), var)
            in tbl

        in let resolve_names tbl l =
            let folder_rn a b = resolve_name a b 
            in List.fold_left folder_rn tbl l
        in let fc state code = 
            match code with
            | StArg((n,t),_)     -> add_var (n,ty t) state 
            | StLocal((n,t),_)   -> add_var (n,ty t) state 
            | StAssign(e1,e2,c)  -> names_of [e1;e2] |> resolve_names state
            | StWhile((e,_),_)   -> names_of [e] |> resolve_names state |> add_scope
            | StIf _             -> state
            | StBranchElse _     -> state
            | StBranch((e,_),c)  -> names_of [e] |> resolve_names state |> add_scope
            | StCall(e,_)        -> names_of [e] |> resolve_names state
            | StRet(e,_)         -> names_of [e] |> resolve_names state
            | StEmpty _  | StBreak _ | StEmit _
            | StContinue _       -> state
        in let uniq_v = List.unique ~cmp:( fun (n,v) (n1,v) -> n = n1 )
        in let merge a b = 
            let s1 = List.length a.lt_vars
            in let s2 = List.length b.lt_vars
            in let trunc l = if s1 = s2 then List.hd l
                             else if (s1+1) = s2 then List.hd (List.tl l)
                             else assert false
            in let ltv_h = (List.hd a.lt_vars) @ (trunc b.lt_vars) |> uniq_v 
            in let ltv_t = List.tl a.lt_vars
            in let ltv   = ltv_h :: ltv_t
            in { lt_vars = ltv }
        in let f st fn =
            let fp = func_props fn
            in fold_code merge fc init (block_props fp.func_code).blk_code 
        in let _  = fold_funcs f init ast
        in resolved |> Hashtbl.of_enum

    type uni_stage = Uni_interim | Uni_final

    let print_constr = List.iter (fun (a,b) -> printf "CONSTR: %s <-> %s\n" (str_of_tp a) (str_of_tp b))

    let unify_no_unwrap constr = List.unique constr |> Types.unify

    let unify uni_stage constr =
        let uni constr = constr |> unify_no_unwrap
        in try uni constr
           with Unify_error(e) -> if uni_stage = Uni_final
                                  then raise_type_error(e)
                                  else constr

    let typeof_name (n,id) ctx = 
        let typeof_name_ (n,id) ctx = 
            let var = Hashtbl.find ctx.c_resolv (n,id)
            in let ty t = try List.assoc t (ctx.c_unify ctx.c_constr) with Not_found -> t
            in unwrap_type (ty var.var_type) ctx.c_ast
        in try
                Hashtbl.find ctx.c_nm_cache (n,id)
           with Not_found ->
                let t = typeof_name_ (n,id) ctx
                in let _ = Hashtbl.replace ctx.c_nm_cache (n,id) t
                in t

    let typeof_name_no_uni (n,id) ctx = 
        let var = Hashtbl.find ctx.c_resolv (n,id)
        in unwrap_type var.var_type ctx.c_ast

    let typeof_field = function TRecField(TRecord(fields),n) -> (try List.assoc n fields
                                                                 with Not_found -> raise (No_field(TRecord(fields),n)) )
                               | x -> assert false 


    let typeof_rec_of_field = function TRecField(TRecord(fields),n) -> TRecord(fields) 
                                      | _ -> assert false 

    let type_replace ctx tp = 
        try List.assoc tp ctx.c_constr with Not_found -> tp

    let typeof_var ctx v = 
        try List.assoc v ( ctx.c_constr |> unify Uni_final )
        with Not_found -> v

    let func_ret_type ctx ft = unwrap_type (type_replace ctx (Types.func_ret_type (type_replace ctx ft))) ctx.c_ast

    let is_function = function
        | TFunEmit _ | TFunNative _ | TFun _ -> true
        | _                                  -> false


    let rec typeof_expr e ctx = 
        let rec typeof_expr' e ctx = match e with
            | EVoid _                -> TVoid
            | ELiteral(LInt _, _)    -> TInt
            | ELiteral(LBool _, _)   -> TBool
            | ELiteral(LString _, _) -> TString
            | EAriphBin _            -> TInt
            | EAriphUn _             -> TInt
            | EBoolUn _              -> TBool
            | EBoolBin _             -> TBool
            | ECmp _                 -> TBool
            | EIdent(n,c)            -> typeof_name (n, id_of c) ctx
            | ECall((e,args),c)      -> let ft = type_replace ctx (typeof_expr' e ctx)
                                        in let _ = assert (is_function ft)
                                        in let fc = check_args c (fun () -> fun_constr ctx ft args)
                                        in let fany = TAny(Parser_ctx.uniq_id())
                                        in let cc = ( (fany, ft) :: ctx.c_constr @ fc) |> unify Uni_final
                                        in let fc2 = List.assoc fany cc |> func_ret_type ctx (* TODO: bad smell here *)
                                        in let prepl = poly_replaces fc2
                                        in let callt = t_repl prepl fc2
                                        in callt
            
            | EList((h,t),c)          -> TList(typeof_expr' h ctx)
            | EListNil _              -> TList(TAny(Parser_ctx.uniq_id()))
            | EPair((a,b),c)          -> TPair(typeof_expr' a ctx, typeof_expr' b ctx)
            | ERecord((n,l),c)        -> typeof_name (n, id_of c) ctx
            | ERecordFieldInit((n,e),c) -> typeof_name (dot n, id_of c) ctx 
            | ERecordField(Rec(r), RecField(f,c2))  -> typeof_name (dot f, id_of c2) ctx |> typeof_field
            | ENothing               -> TVoid
            | EQuot _                -> assert false
    in let tp = typeof_expr' e ctx
    in unwrap_type tp ctx.c_ast
    and fun_constr ctx tp args = match tp with
      | TFunNative(_,a,_) 
      | TFunEmit(_,a,_,_) 
      | TFun(a,_) -> List.map2 ( fun x y -> ((unwrap_type x ctx.c_ast), typeof_expr y ctx) ) a args
      | _ -> []

    let typeof_expr_final e ctx  = 
        type_replace ctx (typeof_expr e ctx)

    let constr_of_rec e ctx =
        let ff  = function ERecord((n,l),_) -> l | _ -> assert false
        in let (tp_r,tp_f)  = let t = typeof_expr e ctx
                              in match t with TRecord(fs) -> (t, fs) | _ -> raise (Invalid_operation_type (t))
        in let unp     = function ERecordFieldInit((name,e),c) -> ((name, id_of c),e) | _ -> assert false
        in let f_rec_t = function TRecField(tp,name) -> tp | _ -> assert false 
        in let flds    = ff e |> List.map unp
        in let flds_e  = List.map (fun ((name,i),e) -> (name,e) ) flds
        in let flds_tp = try List.map ( fun (n,t) -> (n, typeof_expr (List.assoc n flds_e) ctx ) ) tp_f
                         with Not_found -> raise (Invalid_operation_type (tp_r)) 
        in let c1 = (tp_r, TRecord(flds_tp))
        in let c2 = c1 :: List.map (fun ((n,c),_) -> (tp_r, f_rec_t (typeof_name (dot n,c) ctx) ) ) flds
        in c2

    let rec constr_of_expr e ctx =
        let merge a rs = a @ rs
        in let folder_constr s = function
        | EAriphBin(_,(a,b),_)     -> (typeof_expr a ctx, TInt) :: (typeof_expr b ctx, TInt) :: s
        | EAriphUn(_,a,_)          -> (typeof_expr a ctx, TInt) :: s

        | EBoolBin(_,(a,b),_)      -> (typeof_expr a ctx, TBool) :: (typeof_expr b ctx, TBool) :: s
        | EBoolUn(_,a,_)           -> (typeof_expr a ctx, TBool) :: s

        | ECmp(Equal,(a,b),_)
        | ECmp(Unequal,(a,b),_)    -> (typeof_expr a ctx, typeof_expr b ctx) :: s
        | ECmp(x,(a,b),_)          -> (typeof_expr a ctx, TInt) :: (typeof_expr b ctx, TInt) :: s

        | EList((h,t),_)           -> (TList(typeof_expr h ctx), typeof_expr t ctx) :: s
        | ECall((e,args),c)        -> let fn = typeof_expr e ctx
                                      in let dict = fn |> poly_replaces 
                                      in let x = try  check_args c (fun () -> fun_constr ctx fn args @ s)
                                                      |> List.map ( fun (a,b) -> (t_repl dict a, t_repl dict b) )
                                                 with Undefined_fun_call -> []
                                      in x
        | ERecord((n,l),c)         -> (constr_of_rec (ERecord((n,l),c)) ctx) @ s
        | ERecordField(Rec(e),RecField(f,c2)) -> (typeof_expr e ctx, typeof_rec_of_field (typeof_name (dot f, id_of c2) ctx) ) :: s 
        | _               -> s
        in fold_expresion merge folder_constr [] [e]


    let constraints ast ctx = 
        let nctx a = { ctx with c_constr = a }
        in let code_fold acc code =
            let const = match code with 
            | StAssign(e1,e2,_) -> (typeof_expr e1 (nctx acc), typeof_expr e2 (nctx acc)) :: constr_of_expr e1 (nctx acc) @ constr_of_expr e2 (nctx acc)
            | StWhile((e,_),_)  -> (typeof_expr e (nctx acc), TBool) :: constr_of_expr e (nctx acc)
            | StCall(x,_)       -> constr_of_expr x (nctx acc)
            | StBranch((e,_),_) -> (typeof_expr e (nctx acc), TBool) :: constr_of_expr e (nctx acc) @ []
            | _ -> []
            in const @ acc |> ctx.c_unify
        in let folder (total,a) b =
            let fp = func_props b
            in let ft = unwrap_type fp.func_type ctx.c_ast 
            in let c1 = fold_code (fun x y -> y) code_fold a (block_props fp.func_code).blk_code
            in let atomic = of_atom_type (fun x -> x :: []) fp.func_type |> List.map (fun x -> (x,true))
            in let c2 = c1 |> List.filter ( fun (x,_) -> List.mem_assoc x atomic )
            in let ret_fold acc = function StRet(e,_) -> (typeof_expr e ctx) :: acc | _ -> acc
            in let rets = match fp.func_type with
                          | TFunEmit _ -> []
                          | _          -> let r = fold_code (fun a b -> b) ret_fold [] (block_props fp.func_code).blk_code
                                          in if List.length r = 0 then [TVoid] else r
            in let rt = Types.func_ret_type ft
            in let rc = List.map ( fun t -> (rt,t) ) rets
            in (total@c1@rc, c2@rc)
        in let (total,_) = fold_funcs folder ([],[]) ast
        in  { ctx with c_constr = total |> ctx.c_unify }

    let code_id = let d = ref 0 in function () -> d := 1 + !d ; !d;;

    type gen_ctx = { gen_code: line list;
                     gen_ct: ctrl list;
                     gen_vars: (name * gen_var) list list;
                     gen_nest: int;
                     gen_func_end: codeid;
                     gen_func_locals:int;
                   }

    and ctrl = {  loop:(codeid * codeid) option;
                  end_if: codeid option;
                  ctp:ct_tp option;
                }

    and ct_tp = CLoop | CIf | CBr | CBrElse

    and gen_var = GenVar of gen_var_spec

    and gen_var_spec = GLoc of int  | GArg of int | GGlob of (name * int)
    
    let  ct_init (fn,le)   = {  gen_ct = { loop=None; end_if=None; ctp=None; } :: [];
                                gen_code = []; 
                                gen_vars = [[]];
                                gen_nest = 0;
                                gen_func_end = le;
                                gen_func_locals = 0;
                             } 

    let print_loop = function { gen_ct = { loop=Some(a,b) } :: _ }  -> printf "LOOP: %d %d\n" a b | _ -> printf "NO LOOP\n" 

    let codeid_pair () =
        let a = code_id() in let b = code_id()
        in (a,b)

    type branch_t = BrFirst of codeid | BrAny of codeid * codeid  | BrLast of codeid

    let generate_code ast ctx fname = 
        let dv = datavalues ast ctx |> List.map ( fun x -> (x, code_id ()) )

        in let fn_map (_,{var_name=name;var_type=tp}) = ((name,tp),true)
        in let fn_used = ctx.c_resolv |> Hashtbl.enum |> Enum.map fn_map |> Hashtbl.of_enum
        in let fn_filt = function FuncDef({func_name=n;func_type=t},_) -> Hashtbl.mem fn_used (n,t) 
                                 | _ -> assert false

        in let func_table =  ast |> mod_funcs
                                 |> List.map( function FuncDef({func_name=n;func_type=t},_)
                                                           -> ((n,t), codeid_pair() ) 
                                                       | _ -> assert false )
        in let calc_branches b = 
            let ids = b |> List.mapi (fun i (cid,id) -> (i, cid, id) )
            in let rec calc x = match x with
            | (0, c1, x)::(1, c2, y)::rest   -> (c1, BrFirst(y))::(calc ((1, c2, y)::rest)) 
            | (n1, c1, x)::(n2, c2, y)::rest -> (c1, BrAny(x,y))::(calc ((n2, c2, y)::rest))
            | (n1,c1,x)::[]                  -> (c1, BrLast(x))::[]
            | _                              -> []
            in calc ids

        in let count_branches =
            let merge2 a b = if List.length b = 1 then b else a
            in let fold2 acc x = match x with
            | StBranch(_,c)     -> with_head (fun x -> (id_of c, code_id()) :: x) acc
            | StBranchElse(_,c) -> with_head (fun x -> (id_of c, code_id()) :: x) acc
            | StIf _            -> [] :: acc
            | _                 -> acc
            in let fold1 acc x = match x with
            | StIf(ip,c) -> (fold_if merge2 fold2 [[]] ip |> List.hd |> List.rev) :: acc 
            | _          -> acc
            in let folder a b =
                let fp = func_props b
                in if not (fn_filt b || fp.func_name = "main")
                   then a
                   else fold_code (fun _ b -> b) fold1 a (block_props fp.func_code).blk_code
            in fold_funcs folder [] ast

        in let branch_table  = count_branches |> List.map calc_branches |> List.flatten

        in let rec generate_code () = 
            let init = code_initial () |> add_strings
            in let mrg a b = 
                let ct1 = a.gen_ct
                in let ct2 = b.gen_ct
                in let st1 = List.hd ct1
                in let st2 = List.hd ct2
                in let merge_vars a b = (List.hd b.gen_vars :: List.tl a.gen_vars)
                in let vars = if a.gen_nest = b.gen_nest then merge_vars a b else a.gen_vars
                in let tail = if List.length ct1 < List.length ct2 
                              then tail_code st2 else []
                in { a with gen_code = b.gen_code @ tail ; 
                            gen_vars = vars; 
                            gen_func_locals = b.gen_func_locals;
                   }
            in let code_fold st code =
                match code with 
                | StArg((n,t),c)         -> st |> add_arg n 
                | StLocal((n,t),c)       -> st |> add_loc n
                | StAssign(e1,e2,c)      -> st |> assignment e1 e2
                | StWhile((e,_),_)       -> st |> push_loop e |> nest
                | StCall(e,_)            -> st |> add_expr (EVoid(e))
                | StIf({if_elif=ef},_)   -> st |> push_if |> nest
                | StBranch((e,_),c)      -> st |> branch (Some(e)) CBr (id_of c) |> nest
                | StBranchElse(_,c)      -> st |> branch None CBrElse (id_of c)  |> nest
                | StRet(e, _)            -> st |> ret e
                | StContinue _           -> st |> continue
                | StBreak _              -> st |> break
                | StEmpty _              -> st
                | StEmit(l)              -> st |> add_emit l
            in let code_fold_safe st code = try code_fold st code  with x -> raise_compiler_error x code
            in let folder a b =
                let fp = func_props b
                in if not (fn_filt b || fp.func_name = "main")
                   then a
                   else
                       let func = fold_code mrg code_fold_safe (new_ct fp) (block_props fp.func_code).blk_code
                       in gen_wrap_func fp a func
            in let final = fold_funcs folder init ast
            in let _ = gen_executable final fname
            in ()
        and func_end ({func_name=n; func_type=t}) = try List.assoc (n,t) func_table |> snd
                                                    with Not_found -> assert false
    
        and nest st = { st with gen_nest = st.gen_nest + 1; gen_vars = ([]):: st.gen_vars }
        and new_ct fp = ct_init (fp.func_name, func_end fp)
        and add_code code ct = match ct with { gen_code=c } -> { ct with gen_code = ct.gen_code @ code }

        and add_nop ct = ct |> add_code (op NOP :: [])

        and add_emit l ct = ct |> add_code ((List.map (fun x -> op x) l))

        and incr_locals ct = { ct with gen_func_locals = ct.gen_func_locals + 1 }

        and add_arg n ct = add_var n (GArg(count_vars ct)) ct
        and add_loc n ct = ct |> add_var n (GLoc(count_vars ct)) |> incr_locals
        and add_var n t ct = { ct with gen_vars = with_head (fun x -> (n, (GenVar(t)) ) :: x ) ct.gen_vars }
        and count_vars ct = ct.gen_vars |> List.flatten |> List.length 

        and print_vars ct =
            let fl = ct.gen_vars |> List.flatten
            in let _ = List.iter ( function (n, GenVar(GArg(d)))       -> printf "ARG: %s %d\n" n d
                                           |(n, GenVar(GLoc(d)))       -> printf "LOC: %s %d\n" n d
                                           |(n, GenVar(GGlob((s,id)))) -> printf "GLOB: %s %s#%d\n" n s id ) fl
            in ct

        and add_strings (ctx:code_ctx) =
            let data = List.fold_left (fun acc x -> (acc @ make_string x)) [] dv
            in { ctx with c_data = ctx.c_data @ data } 

        and string_id s = try List.assoc s dv with Not_found -> assert false

        and push_loop e ct = let (l1,l2) = (code_id(), code_id())
                             in { ct with gen_ct = {(List.hd ct.gen_ct) with loop = Some((l1,l2)); ctp=Some(CLoop)} :: ct.gen_ct;
                                          gen_code = ct.gen_code @ while_head ct e l1 l2
                                }

        and push_if ct  = { ct with gen_ct = {(List.hd ct.gen_ct) with end_if = Some(code_id()); ctp=Some(CIf)} :: ct.gen_ct;
                          }

        and branch e br id ct = 
            let b = List.assoc id branch_table
            in let self_id = function
            | BrFirst(x) -> None
            | BrAny(x,y) -> Some(x)
            | BrLast(x)  -> Some(x)
            in let next_id = function
            | BrFirst(x) -> x
            | BrAny(x,y) -> y
            | BrLast(x)  -> assert false
            in match (e,br) with
            | (Some(exp), CBr)      -> let code = expr ct exp |> with_head ( fun x -> { x with line_id = (self_id b) } )
                                       in let c  = ct |> add_code ( code @ [op (JZ(next_id b))] )
                                       in { c with gen_ct = {(List.hd ct.gen_ct) with ctp=Some(CBr)}::c.gen_ct } 
            | (None,      CBrElse)  -> let c = ct |> add_code ( op NOP ~id:(self_id b) :: [] )
                                       in { c with gen_ct = {(List.hd ct.gen_ct) with ctp=Some(CBrElse)}::c.gen_ct }
            | _                     -> assert false

        and continue ct = let h = (List.hd ct.gen_ct).loop |> some |> fst
            in ct |> add_code ( op (JMP(h))   ~comment:"continue" :: [] )
        
        and break ct = let h = (List.hd ct.gen_ct).loop |> some |> snd 
            in ct |> add_code ( op (JMP(h)) ~comment:"break" :: [] )

        and ndup n = if n = 0 then [] else (op (NDUP(n))) :: []

        and gen_prologue fp func =
            let (fs,_) = try List.assoc (fp.func_name, fp.func_type) func_table 
                         with Not_found -> failwith fp.func_name
            in (fs, (op (SF (func_arity fp.func_type)) ~id:(Some(fs)) ~comment:fp.func_name) :: ndup func.gen_func_locals )

        and gen_epilogue fp func =
            let (_, fe) = try List.assoc (fp.func_name, fp.func_type) func_table 
                         with Not_found -> failwith fp.func_name
            in let ftr = func_ret_type ctx fp.func_type 
            in let (stmp, ltmp) = match ftr with TVoid -> ([],[]) | x -> (op STMP :: [], op LTMP :: [])
            in let fs = (op (FS (func_arity fp.func_type)) ) :: []
            in let w_id = with_head (fun x -> { x with line_id = Some(fe); comment = sprintf "end of %s" fp.func_name }) 
            in (fe, w_id (stmp @ fs @ ltmp) )

        and gen_wrap_func fp ctx func = match (fp.func_name, fp.func_type) with
            | ("main", _)     -> gen_wrap_func_entry fp ctx func
            | (_, TFun _)     -> gen_wrap_func_plain fp ctx func 
            | (_, TFunEmit _) -> gen_wrap_func_emit fp ctx func
            | _               -> assert false
        and gen_wrap_func_entry fp ctx func  =
            let (id,pro) = gen_prologue fp func
            in let (_,epi) = gen_epilogue fp func
            in { ctx with c_code = ctx.c_code @ pro @ func.gen_code @ epi @ [op DOWN]; c_entry_point = Some(id) }
        and gen_wrap_func_plain fp ctx func =
            let (id,pro) = gen_prologue fp func
            in let (_,epi) = gen_epilogue fp func
            in { ctx with c_code = ctx.c_code @ pro @ func.gen_code @ epi @ [op RET] }
        and gen_wrap_func_emit fp ctx func = 
            let (fs,_) = try List.assoc (fp.func_name, fp.func_type) func_table 
                         with Not_found -> failwith fp.func_name
            in let mark = with_head (fun x -> {x with line_id = Some(fs); comment=fp.func_name})
            in { ctx with c_code = ctx.c_code @ (mark func.gen_code) @ [op RET] }
        and gen_executable ctx fpath =
            let ep = try entry_point ctx with Not_found -> failwith "No entry point"
            in let code = op (JMP ep) :: op NOP ~comment:"align" :: ctx.c_data @ (optimize ctx.c_code)
            in let _ = dump_code_lines code
            in let () = printf "Generated: %d lines OK\n" (List.length ctx.c_code)
            in let codes = binary code |> List.map Char.chr
            in output_file fpath (String.implode codes)
        and tail_code ct = match ct.ctp with
        | Some(CLoop) -> let (a,b) = (function {loop=Some((a,b))} -> (a,b) | _ -> assert false) ct
                         in while_tail a b
        | Some(CIf)         -> op NOP ~id:ct.end_if ~comment:"end if" :: []
        | Some(CBr)         -> op (JMP(some ct.end_if)) ~comment:"-> end if" :: []
        | Some(CBrElse)     -> []
        | None              -> assert false
        and while_head ct e lbegin lend =    (expr ct e |> with_head (function x -> { x with line_id = Some(lbegin) }) )
                                              @ op (JZ(lend)) :: []
        and while_tail lbegin lend   =     op (JMP(lbegin)) 
                                        :: op NOP ~id:(Some(lend))
                                        :: []
        and ret e ct = match e with
            | ENothing    -> ct |> add_code (op (JMP(ct.gen_func_end)) :: [])
            | x           -> ct |> add_expr x |> add_code ( op (JMP(ct.gen_func_end)) :: [])

        and assignment e1 e2 ct = 
            match e1 with
            | EIdent(n,c)     -> ct |> add_expr e2 |> add_code (store (get_var ct (n, id_of c)))
            | ERecordField(Rec(re),RecField(fn,c2)) ->
                                 let fi = typeof_name (dot fn,id_of c2) ctx
                                 in let rt = field_rec_type fi
                                 in let off = rec_field_offset rt fn
                                 in ct |> add_expr re |> add_expr e2 |> add_code (op (TBCWD(off)) ~comment:(dot fn) :: [])

            | other           -> raise (Type_error("lvalue required"))

        and add_expr e ct = ct |> add_code (expr ct e)

        and builtin_by_name name = 
            let nf = try List.find (fun (_, n, f) -> if n = name then true else false ) (Beepvm_builtins.builtins())
                     with Not_found -> raise (Not_resolved name)
            in match nf with 
               (_, _, TFunNative(Some(x), [TString;TString], TBool)) -> op (NCALL x) ~comment:name :: []
               |(_, _, x)                                            -> raise (Invalid_operation_type x)

        and get_var ct (n,id) = 
            try List.assoc n (List.flatten ct.gen_vars)
            with Not_found -> get_global (n,id)

        and get_global (n,id) =
            try 
                let tp = List.assoc n ctx.c_glob 
                in glob_var tp (n,id)
            with Not_found -> raise (Not_resolved n) 

        and glob_var tp (n,id) = 
            match tp with
            | _ -> GenVar(GGlob((n,id)))

        and store t = match t with
            | GenVar(GLoc(0)) | GenVar(GArg(0)) -> op STORE0 :: []
            | GenVar(GLoc(1)) | GenVar(GArg(1)) -> op STORE1 :: []
            | GenVar(GLoc(2)) | GenVar(GArg(2)) -> op STORE2 :: []
            | GenVar(GLoc(3)) | GenVar(GArg(3)) -> op STORE3 :: []
            | GenVar(GLoc(4)) | GenVar(GArg(4)) -> op STORE4 :: []
            | GenVar(GLoc(5)) | GenVar(GArg(5)) -> op STORE5 :: []
            | GenVar(GLoc(6)) | GenVar(GArg(6)) -> op STORE6 :: []
            | GenVar(GLoc(7)) | GenVar(GArg(7)) -> op STORE7 :: []
            | GenVar(GLoc(x)) | GenVar(GArg(x)) -> op (STOREF(x)) :: []
            | GenVar(GGlob(d))                  -> failwith "STORE GLOBAL SYMBOL"

        and load t = match t with
            | GenVar(GLoc(0)) | GenVar(GArg(0)) -> op LOAD0 :: []
            | GenVar(GLoc(1)) | GenVar(GArg(1)) -> op LOAD1 :: []
            | GenVar(GLoc(2)) | GenVar(GArg(2)) -> op LOAD2 :: []
            | GenVar(GLoc(3)) | GenVar(GArg(3)) -> op LOAD3 :: []
            | GenVar(GLoc(4)) | GenVar(GArg(4)) -> op LOAD4 :: []
            | GenVar(GLoc(5)) | GenVar(GArg(5)) -> op LOAD5 :: []
            | GenVar(GLoc(6)) | GenVar(GArg(6)) -> op LOAD6 :: []
            | GenVar(GLoc(7)) | GenVar(GArg(7)) -> op LOAD7 :: []
            | GenVar(GLoc(x)) | GenVar(GArg(x)) -> op (LOADF(x)) :: []
            | GenVar(GGlob(d))                  -> load_global_symbol d

        and load_global_symbol (n,id) =
            let tp = typeof_name (n,id) ctx
            in let offset () = List.assoc (n,tp) func_table |> fst 
            in match tp with
            | TFunEmit(_,a,r,_)
            | TFun(a,r) -> [(op (ADDROF (offset())) ~comment:(sprintf "func %s" n)) ]
            | TFunNative(Some(num),a,r) -> [(op (LIT num) ~comment:(sprintf "%s %s" n (str_of_tp tp) )) ]
            | _         -> failwith (sprintf "THIS OPERATION IS UNSUPPORTED YET FOR: (%s#%d) %s" n id (str_of_tp tp))

        and fun_call e =
            let (rt,callt) = match e with   EVoid(ECall((f,a),c))   -> let tp = typeof_expr_final f ctx
                                                                       in (TVoidCast(func_ret_type ctx tp), tp)
                                            | ECall((f,a),c)        -> let tp = typeof_expr_final f ctx
                                                                       in (func_ret_type ctx tp, tp)
                                            | _                     -> assert false
            in let _ = assert (is_function callt)
(*             in let _ = printf "TYPEOF CALL: %s -- %s\n" (str_of_tp rt) (str_of_tp callt) *)
            in let call = match (rt,callt) with
                          | (TVoidCast(TVoid), TFunEmit _)
                          | (TVoidCast(TVoid), TFun _)     -> op CALLT :: []
                          | (TVoidCast(x), TFunEmit _)
                          | (TVoidCast(x), TFun _)         -> op CALLT :: op DROP :: []
                          | (_, TFunEmit _)
                          | (_, TFun _)                     -> op CALLT :: []

                          | (TVoidCast(TVoid),TFunNative _) -> op NCALLT :: []
                          | (TVoidCast(x),TFunNative _)     -> op NCALLT :: op DROP :: []
                          | (_,TFunNative _)                -> op NCALLT :: []

                          | _                               -> assert false
            in call

        and repl_inc st = 
            List.rev (op INC :: (st |> List.rev |> List.tl))   

        and repl_dec st = 
            List.rev (op DEC :: (st |> List.rev |> List.tl))   

        and expr ct e =
            let merge a b = b
            in let fold_exp st = function
            | ELiteral(LInt(1),_)      -> st @ [op TRUE]
            | ELiteral(LInt(0),_)      -> st @ [op FALSE]
            | ELiteral(LInt(x),_)      -> st @ [op (LIT(x))]
            | ELiteral(LBool(true),_)  -> st @ [op TRUE]
            | ELiteral(LBool(false),_) -> st @ [op FALSE]
            | ELiteral(LString(s),_)   -> st @ [(op (ADDROF(string_id s)) ~comment:(str_to_comment s)) ]

            | EIdent(n,c)              -> st @ (load (get_var ct (n,id_of c)))

            | EAriphUn(Minus, _, _)    -> st @ [op NEG]
            | EAriphUn(BInv, _, _)     -> st @ [op INV]
            | EAriphUn(x,_,_)          -> failwith "SYNTAX ERROR: EAriphUn"
            | EBoolUn(Not, _, _)       -> st @ [op NOT]
            | EBoolUn(x,_,_)           -> failwith "SYNTAX ERROR: EBoolUn"

            | EBoolBin(And,_,_)        -> st @ [op AND] 
            | EBoolBin(Or,_,_)         -> st @ [op OR] 
            | EBoolBin(x,_,_)          -> failwith "SYNTAX ERROR: EBoolBin"

            | EAriphBin(Plus, (_,ELiteral(LInt(1),_)),_) -> repl_inc st
            | EAriphBin(Plus, _, _)    -> st @ [op ADD]


            | EAriphBin(Minus, (_,ELiteral(LInt(1),_)),_) -> repl_dec st 
            | EAriphBin(Minus, _, _)   -> st @ [op SUB]

            | EAriphBin(Mul, _, _)     -> st @ [op MUL]
            | EAriphBin(Div, _, _)     -> st @ [op DIV]
            | EAriphBin(Mod, _, _)     -> st @ [op MOD]
            | EAriphBin(BAnd, _, _)    -> st @ [op AND]
            | EAriphBin(BOr, _, _)     -> st @ [op OR]
            | EAriphBin(BXor, _, _)    -> st @ [op XOR]
            | EAriphBin(BShl, _, _)    -> st @ [op SHL]
            | EAriphBin(BShr, _, _)    -> st @ [op SHR]
            | EAriphBin(x,_,_)         -> failwith "SYNTAX ERROR: EAriphBin"

            | ECmp(Equal, (l,r), _) 
              when (typeof_expr l ctx = TInt && typeof_expr r ctx = TInt)   -> st @ [op EQ]

            | ECmp(Equal, (l,r), _)
              when (typeof_expr l ctx = TBool && typeof_expr r ctx = TBool) -> st @ [op EQ]

            | ECmp(Unequal, (l,r), _) 
              when (typeof_expr l ctx = TInt && typeof_expr r ctx = TInt)   -> st @ [op NEQ]

            | ECmp(Unequal, (l,r), _)
              when (typeof_expr l ctx = TBool && typeof_expr r ctx = TBool) -> st @ [op NEQ]

            | ECmp(Equal, (l,r), _)
              when (typeof_expr l ctx = TString && typeof_expr r ctx = TString) -> st @ builtin_by_name "streq"

            | ECmp(Unequal, (l,r), _) 
              when (typeof_expr l ctx = TString && typeof_expr r ctx = TString) -> st @ builtin_by_name "streq" @ [op NOT]

            | ECmp(Equal, (l,r), _)
            | ECmp(Unequal, (l,r), _) -> raise (Invalid_operation_type (typeof_expr l ctx))

            | ECmp(More, _, _)   -> st @ [op GT]
            | ECmp(MoreEq, _, _) -> st @ [op GEQ]
            | ECmp(Less, _, _)   -> st @ [op LE]
            | ECmp(LessEq, _, _) -> st @ [op LEQ]

            | ECmp(x, _, _)          -> failwith "SYNTAX ERROR: ECmp"

            | EPair _                -> st @ [op CONS ]
            | EList _                -> st @ [op CONS ]
            | EListNil _             -> st @ [op FALSE ]

            | EVoid(ECall((e,a),c))  -> st @ fun_call (EVoid(ECall((e,a),c)))
            | ECall((e,a),c)         -> st @ fun_call (ECall((e,a),c))

            | EVoid(x)               -> assert false


            | ERecord((name,l),c)    -> let tp = typeof_name (name, id_of c) ctx
(*                                         in let _ = printf "RECORD: %s %s\n" name (str_of_tp tp) *)
                                        in let sz = op (LIT(rec_fields_num tp))
                                        in st @ sz :: (op ALLOC ~comment:(sprintf "make record %s" name) :: [])

            | ERecordFieldInit((n,e),c) ->
                                        let tp = typeof_name (dot n,id_of c) ctx |> field_rec_type
                                        in let off = rec_field_offset tp n
                                        in let c = sprintf "setf %s [%d]" (dot n) off
                                        in st @ (op (TBCW(off)) ~comment:c :: [])

            | ERecordField(Rec(re),RecField(fn,c2)) ->
                                        let fi = typeof_name (dot fn,id_of c2) ctx
                                        in let rt = field_rec_type fi
                                        in let off = rec_field_offset rt fn
(*                                         in let _ = printf "ACCESS RECORD FIELD %s (%s)\n" fn (str_of_tp fi) *)
                                        in st @ (op (TBCR(off)) ~comment:(sprintf "getf %s" (dot fn) )  :: [])

            | ENothing               -> []

            | EQuot _                -> assert false

            in fold_expresion merge fold_exp [] [e]
        in generate_code ()

    type prn = { prn_level:int }

    let print_stmt acc stmt =
        let level   = acc.prn_level
        in let _    = open_tbox ()
        in let p s  = print_tbreak (level*4) 0 ; print_string s
        in let acc1 = match stmt with 
        | StArg((n,t),_)    -> sprintf "arg %s : %s\n" n (str_of_tp t)   |> p ; acc
        | StLocal((n,t),_)  -> sprintf "local %s : %s\n" n (str_of_tp t) |> p ; acc
        | StAssign(_,_,_)   -> sprintf "assign _  <- [...]\n"            |> p ; acc
        | StWhile((e,_),_)  -> sprintf "while\n"                         |> p ; { prn_level = acc.prn_level + 1}
        | StIf _            -> sprintf "if\n"                            |> p ; { prn_level = acc.prn_level + 1}
        | StBranchElse(_,c) -> sprintf "else_%d\n" (id_of c)             |> p ; { prn_level = acc.prn_level + 1}
        | StBranch(_,c)     -> sprintf "elif_%d\n" (id_of c)             |> p ; { prn_level = acc.prn_level + 1}
        | _                 -> acc
        in let _ = close_tbox()
        in acc1

    let print_funcs ast = 
        let fc st cc = print_stmt st cc
        in let merge a _ = a
        in let f st fn = 
            let fp = func_props fn
            in let _ = printf "%s : %s\n" fp.func_name (str_of_tp fp.func_type)
            in fold_code merge fc { prn_level=1; } (block_props fp.func_code).blk_code 
        in let _ = fold_funcs f { prn_level=0; } ast 
        in ()

    let print_lookup_table tbl = 
        tbl |> Hashtbl.enum 
            |> Enum.iter (fun ((name,id),var) -> printf "ENTRY: %s#%d -> %s:%s\n" name id var.var_name (str_of_tp var.var_type) ) 

    let emit_func_def t c =
        let ret = function TVoid -> [] | _ -> []
        in match t with 
        | TFunEmit(n,a,r,code) -> FuncDef({func_name=n;func_type=t;func_code=Block({blk_code=([StEmit(code)]@(ret r))},c)},c)
        | _ -> assert false

    let with_builtins = function Module(p,c) ->
        let bu = builtins () |> List.map (function   (_,n,TFunNative(num,a,r)) -> ExternFunc((n, TFunNative(num,a,r)), c)
                                                   | (_,n,TFunEmit(nm,a,r,code)) -> emit_func_def (TFunEmit(nm,a,r,code)) c
                                                   | _ -> assert false )
        in Module({mod_defs = bu @ p.mod_defs},c)

    let rec with_funcs f l = match l with
        | (FuncDef(fp,c))::ds -> (f (FuncDef(fp,c))) :: (with_funcs f ds)
        | d::ds               -> d :: (with_funcs f ds)
        | []                  -> []

    let expand_macros (Module({mod_defs=defs},c)) = 
        let ql = List.map (function MacroDef(MacroLiteral(name,e),c) -> (name,e)::[] | _ -> [])
        in let equots = defs |> ql |> List.fold_left (@) []
        in let rec exp_macro_expr e = match e with 
        | ENothing                 ->  ENothing
        | ELiteral(n,c)            -> (ELiteral(n,c))
        | EListNil(c)              -> (EListNil(c))
        | EIdent(n,c)              -> (EIdent(n,c)) 
        | EVoid(ECall((e,a),c))    -> (EVoid(ECall((exp_macro_expr e, List.map exp_macro_expr a),c)))
        | ECall((e,a),c)           -> (ECall((exp_macro_expr e, List.map exp_macro_expr a),c))
        | EVoid(x)                 -> (EVoid(x))
        | ERecord((n,ef),c)        -> (ERecord((n,List.map exp_macro_expr ef),c))
        | ERecordFieldInit((n,e),c)-> (ERecordFieldInit((n,exp_macro_expr e),c))
        | ERecordField(Rec(e),x)   -> (ERecordField(Rec(exp_macro_expr e),x))
        | EBoolUn(op, e, c)        -> (EBoolUn(op, exp_macro_expr e, c))
        | EAriphUn(op, e,c)        -> (EAriphUn(op, exp_macro_expr e,c))
        | EPair( (e1, e2), c )     -> (EPair( (exp_macro_expr e1, exp_macro_expr e2), c ))
        | EList( (e1, e2), c )     -> (EList( (exp_macro_expr e1, exp_macro_expr e2), c ))
        | ECmp(op, (e1,e2), c )    -> (ECmp(op, (exp_macro_expr e1, exp_macro_expr e2), c ))
        | EBoolBin(op, (e1,e2), c) -> (EBoolBin(op, (exp_macro_expr e1, exp_macro_expr e2), c))
        | EAriphBin(op, (e1,e2),c) -> (EAriphBin(op, (exp_macro_expr e1, exp_macro_expr e2),c))
        | EQuot(n,c)               -> check_name n c (fun () -> List.assoc n equots)

        in let rec exp_macro_stmt stmt = match stmt with 
        | StArg _                                    
        | StLocal _ 
        | StEmpty _         
        | StBreak _ 
        | StEmit _
        | StContinue _                              -> stmt
        | StIf({if_then=s1;if_elif=s2;if_else=s3},c)-> let c1 = exp_macro_stmt s1
                                                       in let c2 = List.map exp_macro_stmt s2
                                                       in let c3 = exp_macro_stmt s3
                                                       in StIf({if_then=c1;if_elif=c2;if_else=c3},c)
        | StBranchElse(Block({blk_code=code},c1),c) -> StBranchElse(Block({blk_code=List.map exp_macro_stmt code},c1),c)
        | StAssign(x,e,c)                           -> StAssign(x, exp_macro_expr e,c)    
        | StWhile((e,Block({blk_code=code},c1)),c)  -> StWhile((exp_macro_expr e, Block({blk_code=List.map exp_macro_stmt code},c1) ),c)
        | StBranch((e,Block({blk_code=code},c1)),c) -> StBranch((exp_macro_expr e, Block({blk_code=List.map exp_macro_stmt code},c1)),c) 
        | StCall(e,c)                               -> StCall(exp_macro_expr e,c)
        | StRet(e,c)                                -> StRet(exp_macro_expr e,c)
        in let exp_macro_code (Block({blk_code=code},c)) = 
           Block({blk_code=List.map exp_macro_stmt code},c)
        in let exp_macro_fun fn = match fn with 
            | FuncDef(fp,c) -> FuncDef({fp with func_code=exp_macro_code fp.func_code},c)
            | _             -> assert false
        in Module({mod_defs=with_funcs exp_macro_fun defs},c)

    let expand_typenames ast =
        let unwrap = with_atom_type (function x -> unwrap_type x ast)
        in let rec exp_tn_stmt stmt = match stmt with
        | StArg((n,t),c)   -> StArg((n, unwrap t),c)
        | StLocal((n,t),c) -> StLocal((n, unwrap t),c)
        | StIf({if_then=s1;if_elif=s2;if_else=s3},c)-> let c1 = exp_tn_stmt s1
                                                       in let c2 = List.map exp_tn_stmt s2
                                                       in let c3 = exp_tn_stmt s3
                                                       in StIf({if_then=c1;if_elif=c2;if_else=c3},c)
        | StBranchElse(Block({blk_code=code},c1),c) -> StBranchElse(Block({blk_code=List.map exp_tn_stmt code},c1),c)
        | StWhile((e,Block({blk_code=code},c1)),c)  -> StWhile((e, Block({blk_code=List.map exp_tn_stmt code},c1) ),c)
        | StBranch((e,Block({blk_code=code},c1)),c) -> StBranch((e, Block({blk_code=List.map exp_tn_stmt code},c1)),c) 
        | StEmpty _         
        | StBreak _ 
        | StEmit _
        | StRet _
        | StAssign _
        | StCall _
        | StContinue _  -> stmt
        in let exp_tn_code (Block({blk_code=code},c)) = 
           Block({blk_code=List.map exp_tn_stmt code},c)
        in let exp_tn (Module({mod_defs=defs},c)) = 
            let exp_typename_fun fn = match fn with 
                | FuncDef(fp,c) -> FuncDef({fp with func_type = unwrap fp.func_type;
                                                    func_code = exp_tn_code fp.func_code} ,c)
                | _             -> assert false
            in Module({mod_defs=with_funcs exp_typename_fun defs},c)
        in exp_tn ast

    let expand_types ast = 
        let mk x = match x with TypeDef((name,tp),_)  ->  ( TTypename(name), tp)
                                | _ -> assert false
        in let typedefs = mod_typedefs ast
        in let tds = typedefs |> List.map mk |> List.enum |> Hashtbl.of_enum

        in let repl t = try Hashtbl.find tds t with Not_found -> t
        in let tr = with_atom_type repl

        in let rewrite_types = function   FuncDef(fp,c)    -> FuncDef({ fp with func_type = tr fp.func_type } ,c) 
                                        | TypeDef((n,t),c) -> TypeDef((n, tr t),c)
                                        | ExternFunc((n,t),c)  -> ExternFunc((n, tr t),c)
                                        | MacroDef(x,c)    -> MacroDef(x,c)

        in let defs = mod_defs ast |> List.map rewrite_types
        in let update  = function Module({mod_defs=d},c) -> Module({mod_defs=defs},c)
        in update ast

    let normalize_ext_funcs (Module({mod_defs=defs},c)) = 
        let (others, ext_funcs) = List.partition (function ExternFunc _ -> false | _ -> true) defs
        in let normalized v = 
            let cmp a b = match a,b with
            | ExternFunc((n,TFunNative(Some(x),_,_)),_), ExternFunc((n',TFunNative(Some(x'),_,_)),_) -> compare x x'
            | ExternFunc((n,TFunNative(None,_,_)),_), ExternFunc((n',TFunNative(Some(x'),_,_)),_) -> 1
            | ExternFunc((n',TFunNative(Some(x'),_,_)),_), ExternFunc((n,TFunNative(None,_,_)),_) -> (-1)
            | ExternFunc((n,TFunNative(None,_,_)),_), ExternFunc((n',TFunNative(None,_,_)),_)     -> 0
            | _ -> assert false
            in let sorted = List.sort ~cmp:cmp v
            in let rec process n l = match l with
            | ExternFunc((nm, TFunNative(Some(x), x1, x2)),c) :: xs -> ExternFunc((nm, TFunNative(Some(x),  x1, x2)),c) :: process x xs
            | ExternFunc((nm, TFunNative(None, x1, x2)),c)    :: xs -> ExternFunc((nm, TFunNative(Some(n+1), x1, x2)),c) :: process (n+1) xs
            | z :: xs                                           -> assert false
            | []                                                -> []
            in let validate x = 
                let rec validate_rec dict rest = 
                match rest with 
                | ExternFunc((nm, TFunNative(Some(x), x1, x2)),c) :: xs -> let _ = happened dict x nm c 
                                                                          in validate_rec ((x, (nm,c)) :: dict) xs
                | x :: xs                                               -> assert false
                | []                                                    -> x
                and happened dict v nm c = 
                    let e = try Some(List.assoc v dict) with Not_found -> None
                    in match e with
                    | Some((nm',_)) -> raise (CompilerError(NameHideError(nm, nm'), err_context c))
                    | _            -> ()
                in validate_rec [] x
            in sorted |> process 0 |> validate
        in let defs' = others @ normalized ext_funcs
        in Module({mod_defs=defs'},c)

    let filename = function None -> "a.out" | Some(x) -> x

    let generate_stubs out_f ctx = 
        let gen_stubs stubs_name = 
            let stubs = stubs_create stubs_name
            in let fld acc = function TypeDef((name,TRecord(fields)),_) -> (name, TRecord(fields)) :: acc | _ -> acc
            in let flde acc = function ExternFunc((name,TFunNative(n,a,r)),_) -> (name,TFunNative(n,a,r)) :: acc | _ -> acc
            in let fld_rec stb = function (name, TRecord(fields)) -> stubs_record_header name fields stb | _  -> stb
            in let fld_ext stb = function (name, TFunNative(n,a,r)) -> stubs_builtin (name, TFunNative(n,a,r)) stb | _  -> stb
            in let recs = ctx.c_ast |> mod_typedefs  |> List.fold_left fld  []
            in let exts = ctx.c_ast |> mod_funcs_ext |> List.fold_left flde []
            in let stubs2 = List.fold_left fld_ext (List.fold_left fld_rec stubs recs) exts
            in stubs2 |> stubs_write
        in match out_f with
        | None    -> ()
        | Some(x) -> gen_stubs x

    let compile_ast ?fname:(fn=None) ?fstubs:(stubs=None) ast' =
        let () = print_endline "Compile AST"
        in let ast1 = with_builtins ast' |> expand_types
        in let t0 = Unix.gettimeofday()
        in let ast = ast1 |> expand_macros |> expand_typenames |> normalize_ext_funcs
(*         in let _ = print_funcs ast *)
(*        in let _ = printf "MACRO: %f\n" (Unix.gettimeofday() -. t0)*)
(*         in let _  = print_funcs ast  *)
        in let initial = globals ast
        in let tbl  = lookup_table ast initial
         in let _ = print_lookup_table tbl 
        in let t1 = Unix.gettimeofday()
        in let ctx2 = constraints ast { c_resolv = tbl;
                                        c_glob = initial;
                                        c_ast = ast; 
                                        c_constr = [];
                                        c_unify = unify Uni_final;
                                        c_nm_cache = Hashtbl.create 2000;
                                      }
(*        in let _ = printf "CONSTR: %f\n" (Unix.gettimeofday() -. t1)*)
(*        in let _ = printf "CONSTR LENGTH: %d\n" (List.length ctx2.c_constr)*)
(*        in let _ = print_constr ctx2.c_constr*)
        in let t2 = Unix.gettimeofday()
(*         in let _ = generate_code ast { ctx2 with c_unify = (fun x -> x) } (filename fn) *)
        in let _ = generate_code ast { ctx2 with c_unify = (fun x -> x) } (filename fn)
        in let _ = printf "GEN: %f\n" (Unix.gettimeofday() -. t2)
        in let _ = generate_stubs stubs ctx2

      in ()

    let compile_file ?fstubs:(stubs=None) in_f out_f =
        let lex = in_f |> input_file |> Message.lexer_from_string ~fn:(Some(in_f))
        in try
            let ast  = Parser.toplevel Lexer.token lex
            in let _ = compile_ast ~fname:(Some out_f) ~fstubs:stubs ast
            in ()
        with Parsing.Parse_error -> raise_parse_error lex 

end
