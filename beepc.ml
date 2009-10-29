open Printf
open Util
open Std
open ExtList
open Compiler

let (|>) f x = x f 

let s_file_name x     = (Filename.chop_extension (Filename.basename x)) ^ ".s"
let out_file_name x   = (Filename.chop_extension (Filename.basename x)) ^ ".bin"
let stubs_file_name x = (Filename.chop_extension (Filename.basename x)) ^ "_stubs"

let in_file_name = Sys.argv.(1)

type opts  = {
    mutable verbose : bool;
    mutable file    : string;
}

exception DoSkel
exception DoArray
exception DoEnum

let _ =
    try
        let o = { verbose = false; file = "" }
        in let _ = Arg.parse [
                                ("--enum",    Arg.Unit(fun () -> raise DoEnum  ), "generate vm enum");
                                ("--skel",    Arg.Unit(fun () -> raise DoSkel  ), "generate vm skeleton");
                                ("--array",   Arg.Unit(fun () -> raise DoArray ), "generate vm bytecode array");
                                ("--verbose", Arg.Unit(fun () -> o.verbose <- true                  ), "be verbose");
                             ] (fun x -> o.file <- x ) "Usage:"
        in let co = { Compiler.comp_verbose=o.verbose;
                      Compiler.comp_stubs=Some((stubs_file_name o.file))
                    }
        in Compiler.compile_file co o.file (out_file_name o.file)

    with 
    | DoSkel  -> Beepvm.gen_skel ()
    | DoArray -> Beepvm.gen_array ()
    | DoEnum  -> Beepvm.gen_enum ()
    | err -> print_endline (Errors.string_of_error err) ; failwith "Aborted"

