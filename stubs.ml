open ExtList
open Printf
open Util
open Std

type stubs = { stubs_name:string; stubs_lines:string list; stubs_builtins:(string*Types.beep_type) list }

let stubs_create name = { stubs_name = name; stubs_lines = []; stubs_builtins = [] }

let stubs_header acc = let g = sprintf  "__beep_stubs_%s__" acc.stubs_name
                        in let lines = sprintf "#ifndef %s" g :: sprintf "#define %s" g :: []
                        in { acc with stubs_lines = lines @ acc.stubs_lines  }

let stubs_footer acc  = { acc with stubs_lines = acc.stubs_lines @ "" :: [sprintf "#endif"; ""] }

let stubs_record_fields fs =
    let tr = Types.TRecord(fs)
    in let cmp a b = compare (Syntax.rec_field_offset tr a) (Syntax.rec_field_offset tr b)
    in List.map fst fs |> List.sort ~cmp:cmp |> List.map (fun s -> sprintf "uint16_t %s;" s)

let stubs_builtin (name, tp) acc = 
    { acc with stubs_builtins = (name, tp) :: acc.stubs_builtins }

let stubs_record_header name fields acc = { acc with stubs_lines = acc.stubs_lines
                                                                   @ ""
                                                                  :: "typedef union {"
                                                                  :: sprintf "uint16_t %s_fields[%d];" name (List.length fields)
                                                                  :: sprintf "struct {"
                                                                  :: stubs_record_fields fields
                                                                   @ sprintf "};"
                                                                  :: sprintf "} beep_%s_t;" name
                                                                  :: sprintf "#define DEF_%s 1" name
                                                                  :: "" 
                                                                  :: []
                                         }
                                                                                 

let beepvm_prefix i s = sprintf "/* 0x%02X: */ BEEPVM_FUNC_NAME(%s)" i s

let stubs_gen_builtins acc =
    let cmp a b = match (a,b) with
    | ((_,Types.TFunNative(n1,_,_)),(_,Types.TFunNative(n2,_,_))) -> compare n2 n1
    | _ -> assert false
    in let sorted = List.sort ~cmp:cmp acc.stubs_builtins
    in let bdict = List.map (function (name,Types.TFunNative(Some(num),_,_)) -> (num, name) | _ -> assert false ) sorted
    in let bnum = 1 + (function (_, Types.TFunNative(Some(n),_,_)) -> n | _ -> assert false)(List.hd sorted)
    in let en = Enum.init bnum ( fun i -> try beepvm_prefix i (List.assoc i bdict) 
                                          with  Not_found -> beepvm_prefix i "nothing" ) 
                |> List.of_enum |> String.concat ",\n"
    in let decls = List.map (fun (num,name) -> sprintf "BEEPVM_FUNC_EXTERN_DECL(%s);" name) bdict 
    in    sprintf "/* %s.c */" acc.stubs_name
       :: "#include <beepvm_builtins.h>"
       :: ""
       :: decls
        @ ""
       :: "const BEEPVM_BUITIN _builtins[BUILTINS_NUM] = {"
       :: en
       :: "};"
       :: ""
       :: [] |> String.concat "\n"

let stubs_write acc =
    let acc' = acc |> stubs_header |> stubs_footer
    in let content = sprintf "/* %s */\n" acc'.stubs_name :: acc'.stubs_lines   |> String.concat "\n"

    in let builtins = Std.output_file (acc.stubs_name ^ ".c") (stubs_gen_builtins acc')

    in Std.output_file (acc.stubs_name ^ ".h") content

