open Printf
open Compiler.Compiler

let fn = function Some(x) -> x | None -> ""

let str_of_ctx { comp_err_fname = x; comp_err_lnum = n } = sprintf "%s:%d" (fn x) n

let string_of_error x = match x with
    | CompilerError(SyntaxError, c)   -> sprintf "*** Error [Syntax] at %s" (str_of_ctx c)
    | CompilerError(TypeError(s), c)  -> sprintf "*** Error [Typing] %s at %s" s (str_of_ctx c)
    | CompilerError(NameError(s), c)  -> sprintf "*** Error [Name] %s at %s" s (str_of_ctx c)
    | CompilerError(OtherError(s), c) -> sprintf "*** Error [Other] %s at %s" s (str_of_ctx c)
    | x                               -> raise x 

