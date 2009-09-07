open Printf
open Opcodes
open Util

type codeid = int
type line   = { line_id:codeid option; opcode:opcode; comment:string }
type code_ctx = { c_sctx: int list;
                  c_code:line list;
                  c_data:line list;
                  c_entry_point:codeid option;
                  }

let code_initial () = { c_sctx = []; c_code = []; c_data=[]; c_entry_point = None }

let entry_point ctx = match ctx.c_entry_point with 
                        Some(n) -> n
                      | None    -> raise Not_found 

let hi_lo x = [(x lsr 8) land 0xFF; x land 0xFF] 
let lo_hi x = [x land 0xFF; (x lsr 8) land 0xFF]
let endian x = lo_hi x
let hi_endian_bytes b0 b1 = (b1 lsl 8) lor b0
let endian_bytes b0 b1 = hi_endian_bytes

let op ?comment:(c="") ?id:(x=None) opcode = {line_id=x; opcode=opcode; comment=c}

let str_of_line_id = function None -> "" | Some n -> sprintf "%04X" n

let str_of_line = function {line_id=x;opcode=opc;comment=c} -> sprintf "%4s %-16s ; %s" 
                                                                        (str_of_line_id x) 
                                                                        (sprintf "%-8s %4s" (str_of_opcode opc) (str_of_lit opc))
                                                                        c
let dump_code_lines = List.iter (fun x -> print_endline (str_of_line x))

let hi_lo x = [(x lsr 8) land 0xFF; x land 0xFF] 
let lo_hi x = [x land 0xFF; (x lsr 8) land 0xFF]

let str_of_code ?cmnt:(cm=true) c =
    let str_of_lit = function
    | EMIT   x -> sprintf "[...]"
    | DATA   x -> sprintf "%04X" x
    | ADDROF x -> sprintf "%04X" x
    | SF     x -> sprintf "%04X" x
    | FS     x -> sprintf "%04X" x
    | TBCW   x -> sprintf "%04X" x
    | TBCR   x -> sprintf "%04X" x
    | LIT    x -> sprintf "%04X" x
    | LOADF  x -> sprintf "%04X" x
    | STOREF x -> sprintf "%04X" x
    | JMP    x -> sprintf "%04X" x
    | JNZ    x -> sprintf "%04X" x
    | JZ     x -> sprintf "%04X" x
    | CALL   x -> sprintf "%04X" x
    | NCALL  x -> sprintf "%04X" x
    | LW     x -> sprintf "%04X" x
    | NDUP   x -> sprintf "%04X" x
    | _        -> ""
    in sprintf "%5s %-6s %4s ; %s"  (match c.line_id with Some(x) -> sprintf "%04X:" x | _ -> "" )
                                    (str_of_opcode c.opcode)
                                    (str_of_lit c.opcode)
                                    (if cm then c.comment else "")

let binary code = 
    let getcode x = Opcodes.code x
    in let emit data = List.map getcode data
    in let rec item_size x = match x with
        | DATA x                                 -> 2
        | EMIT x                                 -> List.length (emit x)
        | ADDROF x                               -> item_size (LIT 0)
        | x when (has_literal x) || (has_addr x) -> 3
        | _                                      -> 1
    in let rec offsets acc l = match l with
        | {line_id=None;opcode=op}::xs -> offsets (acc+item_size op) xs 
        | {line_id=Some(x);opcode=op}::xs -> (x, acc) :: offsets (acc + item_size op) xs
        | []                   -> []

    in let encode = lo_hi 

    in let offs = offsets 0 code

            in let bin = function
                | {opcode=DATA(x)}               -> encode x
                | {opcode=EMIT(x)}               -> emit x
                | {opcode=ADDROF(x)}             -> getcode (LIT 0) :: encode (List.assoc x offs)
                | {opcode=x} when has_addr(x)    -> getcode(x)      :: encode (List.assoc (get_addr x) offs)
                | {opcode=x} when has_literal(x) -> getcode(x)      :: encode (get_literal x)
                | {opcode=x}                     -> getcode(x)      :: []

(*             in let _ = dump_code_lines code *)
        (*     in let () = List.iter (fun x -> printf "%04X : %04X\n" (fst x) (snd x) ) offs  *)
            in let bincode = List.fold_left (fun acc c -> acc @ bin c ) [] code |> 
                            (function x -> if (List.length x) mod 2 == 0 then x else x @ [getcode NOP] )
            in let lookup c  = match c with
            | { line_id=None }    -> 0xFFFF
            | { line_id=Some(x) } -> try List.assoc x offs with Not_found -> 0xFFFF
(*            in let bincode2 = List.map (fun c -> ((c, lookup c), bin c)) code*)
            in let hex l = String.concat " " (List.map (fun x -> sprintf "%02X" x) l)

(*
            in let() = List.iter (fun ((c, off), b) -> printf "%04X {%d} {%d} %-16s[%-4s] %s \n"  
                                                                                             off  (List.length b)
                                                                                             (item_size c.opcode)
                                                                                             (str_of_code c ~cmnt:false) 
                                                                                             (hex b) c.comment) bincode2
*)



            in bincode

let words_of_str s = 
    let rec words s =
        let len = String.length s
        in let ord = Char.code 
        in match len with
        | 0  -> []
        | 1  -> [ hi_endian_bytes (ord s.[0]) 0]
        | 2  -> [ hi_endian_bytes (ord s.[0]) (ord s.[1])]
        | _  ->   hi_endian_bytes (ord s.[0]) (ord s.[1]) :: words (String.sub s 2 (len-2))       

    in words s

let str_to_comment s =
    let len = String.length s
    in let fmt s = sprintf "[%d:'%s']" len (String.escaped s)
    in if len <= 16 then fmt s else (String.sub s 0 13) ^ "..." |> fmt

let make_string (s,id) = 
    let len = String.length s
    in let comment = str_to_comment s
    in let hdr x = op (DATA len) :: x
    in let code = words_of_str s |> List.map ( fun w -> op (DATA w) ) |> hdr
                                 |> with_head (fun x -> {x with line_id = Some(id); comment=comment})
    in code


