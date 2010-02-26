open Printf
open Opcodes
open Code
open Util

let optimize c idgen verbose =
    let rec opt_rev c = match c with
    | op1::op2::ops -> let ss = opt_pair (op1, op2)
                       in if List.length ss > 0 then ss @ opt_rev ops else op1 :: opt_rev (op2 :: ops)
    | op1::[]       -> op1 :: []
    | []            -> []
    and opt_pair (a,b) = match (a,b) with 
    | ({opcode=JMP(x1);comment=c1;line_id=None},
       {opcode=JMP(x2);comment=c2;line_id=None})   -> {a with opcode = JMP(x2);comment=c1^"/"^c2;} :: []

    | ({opcode=NCALLT;comment=c1;line_id=None},
       {opcode=LIT(x);comment=c2;line_id=n})       -> {opcode = NCALL(x);comment=c2;line_id=n} :: []

    | ({opcode=CALLT;comment=c1;line_id=None},
       {opcode=ADDROF(x);comment=c2;line_id=n})    -> {opcode = CALL(x);comment=c2;line_id=n} :: []

    | ({opcode=JZ(n);line_id=None;},
       {opcode=NOT;line_id=None})                  -> { a with opcode = JNZ(n) } :: []
    | ({opcode=op;line_id=Some(n1)},
       {opcode=JMP(n2);line_id=None})              -> if n1 == n2 then a :: [] else []
    | _                                            -> []

 
    in let opt_fun_tails code new_code     =
        let rec opt_fun_tails_r code = match code with
        | {opcode=RET} :: {opcode=LTMP} :: {opcode=FS(n)} :: {opcode=STMP} :: xs -> failwith "FIND SEQUENCE!"
        | {opcode=RET} :: {opcode=FS(n)} :: xs                                   -> failwith "FIND SOMETHING ELSE!"
        | x :: xs                             -> opt_fun_tails_r xs
        | []                                  -> code
        in opt_fun_tails_r code


    in let update_jumps code repl_tbl =
        let repl n = List.assoc n repl_tbl
        in let subst op =
        try
            match op with
            | {opcode=JMP(n)}  -> { op with opcode = JMP(repl n) }
            | {opcode=JNZ(n)}  -> { op with opcode = JNZ(repl n) }
            | {opcode=JZ(n)}   -> { op with opcode = JZ(repl n) }
            | {opcode=CALL(n)} -> { op with opcode = CALL(repl n)}
            | x                -> x
        with Not_found -> op
        in List.map subst code

    in let rc = List.rev
(*     in let _ = dump_code_lines rc *)
    in let remove_nops code =
        let rec remove_nops_rec code ncode nops tbl =
        match code with
        | {line_id=Some(n);opcode=NOP;}::xs  -> remove_nops_rec xs ncode (n::nops) tbl
        | {line_id=None;   opcode=NOP;}::xs  -> remove_nops_rec xs ncode nops tbl
        | x::xs                              -> if nops == [] 
                                                then remove_nops_rec xs (ncode @ [x]) [] tbl
                                                else
                                                    let nid = match x.line_id with Some(n) -> n | None -> idgen()
                                                    in remove_nops_rec xs (ncode @ [{x with line_id=Some(nid);}]) [] 
                                                                       (List.fold_left (fun acc nop -> (nop,nid) :: acc) tbl nops)
        | []                                 -> (ncode, tbl)
        in let (new_code, repl_tbl) = remove_nops_rec code [] [] []
        in let () = if verbose then List.iter (fun (nop,nid) -> printf "JUMP REPLACE: %04X -> %04X\n" nop nid) repl_tbl
        in update_jumps new_code repl_tbl

    in let opt = remove_nops c |> rc |> opt_rev (*|>  opt_fun_tails*) |> List.rev
(*     in let _ = print_endline "" ; print_endline "" *)
(*     in let _ = dump_code_lines opt *)
    in opt 

