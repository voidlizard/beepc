open Genlex
open Printf
open ExtList
open Util

exception Not_opcode
exception Not_builtin

type builtin_spec = { bn_idx     : int option;
                      bn_name    : string;
                      bn_vm_name : string;
                      bn_params  : int;
                      bn_emit    : string list }

type opcode = Op of string * int option | Builtin of string * builtin_spec


let mk_spec name vm_name pn emit = 
    { bn_idx = None; bn_name = name; bn_vm_name = vm_name; bn_params = pn ; bn_emit = emit }

let lexer stream = make_lexer ["OPCODE";"BUILTIN";"EMIT";"(";")"] stream

let opcmp a b = 
    let nvl = function 
        | Op(_,Some(x)) -> x
        | Op(_,None)    -> 0
        | _             -> 0
    in  compare (nvl a) (nvl b)

let rec parse_stream = parser
    |[<x = parse_opcode; xs = parse_stream>] -> x::xs
    |[<>] -> [] 
and parse_opcode = parser
    |[<'Kwd "OPCODE";  'Ident s; x = parse_code>] -> Op(s, x) 
    |[<'Kwd "BUILTIN"; 'Ident a; 'Ident b; 'Kwd "("; 'Int pn; 'Kwd ")"; emit = parse_emit>] -> Builtin(b, (mk_spec b a pn emit) ) 
and parse_code = parser 
    |[<'Int code>] -> Some code
    |[<>]          -> None
and parse_emit = parser
    |[<'Kwd "EMIT"; 'Kwd "("; 'Ident emit; emits = parse_wl; 'Kwd ")">] -> emit::emits
    |[<>]                                                                 -> []
and parse_wl = parser
    |[<'Ident h; t=parse_wl>] -> h::t
    |[<>]                     -> []

let builtins ll = 
    let builtin = function Builtin(_,_) -> true | _ -> false
    in List.mapi (fun i x -> match x with 
                             | Builtin(n, sp) -> Builtin(n, { sp with bn_idx = Some(i) })
                             | _ -> raise Not_builtin) 
                 (List.filter builtin ll)

let ops_numbered l =
    let op = function | Op(_,_) -> true | _ -> false
    in let ops = List.filter  op l
    in let rec op_num ll n = match ll with
    | Op(x, None)::xs   -> Op(x, Some n) :: op_num xs (n+1)
    | Op(x, Some m)::xs -> Op(x, Some m) :: op_num xs (m+1)
    | _::xs             -> op_num xs (n)
    | []                -> [] 
    in List.sort (op_num ops 0) ~cmp:opcmp 

let parsed_spec s = lexer s |> parse_stream

let opcodes  = ops_numbered

