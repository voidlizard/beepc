open Printf
open Util
open Std
open Compiler

let s_file_name x     = (Filename.chop_extension (Filename.basename x)) ^ ".s"
let out_file_name x   = (Filename.chop_extension (Filename.basename x)) ^ ".bin"
let stubs_file_name x = (Filename.chop_extension (Filename.basename x)) ^ "_stubs"

let in_file_name = Sys.argv.(1)

let _ = 
    try
        match Sys.argv.(1) with
        | "-enum"  -> Beepvm.gen_enum ()
        | "-skel"  -> Beepvm.gen_skel ()
        | "-array" -> Beepvm.gen_array ()
        | x        -> Compiler.compile_file x (out_file_name x) ~fstubs:(Some((stubs_file_name x)))
    with err -> print_endline (Errors.string_of_error err) ; failwith "Aborted"

