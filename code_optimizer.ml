open Printf
open Opcodes
open Code
open Util

let optimize c idgen =
    let rec opt_rev c = match c with
    | op1::op2::ops -> let ss = opt_pair (op1, op2)
                       in if List.length ss > 0 then ss @ opt_rev ops else op1 :: opt_rev (op2 :: ops)
    | op1::[]       -> op1 :: []
    | []            -> []
    and opt_pair (a,b) = match (a,b) with 
    | ({opcode=NCALLT;comment=c1;line_id=None},
       {opcode=LIT(x);comment=c2;line_id=None})    -> {a with opcode = NCALL(x);comment=c2;} :: []
    | ({opcode=CALLT;comment=c1;line_id=None},
       {opcode=ADDROF(x);comment=c2;line_id=None}) -> {a with opcode = CALL(x);comment=c2;} :: []
    | ({opcode=JMP(x1);comment=c1;line_id=None},
       {opcode=JMP(x2);comment=c2;line_id=None})   -> {a with opcode = JMP(x2);comment=c1^"/"^c2;} :: []
    | _                                            -> []
    in let rc = List.rev
(*     in let _ = dump_code_lines rc *)
    in let remove_nops code =
        let rec remove_nops_rec code ncode nops tbl =
        match code with
        | {line_id=Some(n);opcode=NOP;}::xs  -> remove_nops_rec xs ncode (n::nops) tbl
        | {line_id=None;   opcode=NOP;}::xs  -> remove_nops_rec xs ncode nops tbl
        | x::xs                              -> if List.length nops == 0
                                                then remove_nops_rec xs (ncode @ [x]) [] tbl
                                                else
                                                    let nid = match x.line_id with Some(n) -> n | None -> idgen()
                                                    in remove_nops_rec xs (ncode @ [{x with line_id=Some(nid);}]) [] 
                                                                       (List.fold_left (fun acc nop -> (nop,nid) :: acc) tbl nops)
        | []                                 -> (ncode, tbl)
        in let (new_code, repl_tbl) = remove_nops_rec code [] [] []
        in let _ = List.iter (fun (nop,nid) -> printf "JUMP REPLACE: %04X -> %04X\n" nop nid) repl_tbl
        in let repl n = try List.assoc n repl_tbl with Not_found -> failwith (sprintf "NOT FOUND LBL: %04X" n)
        in let has_repl n = List.mem_assoc n repl_tbl
        in new_code |> List.map (fun x -> match x with
                                         | {opcode=JMP(n)}  when has_repl n -> { x with opcode = JMP(repl n) }
                                         | {opcode=JNZ(n)}  when has_repl n -> { x with opcode = JNZ(repl n) }
                                         | {opcode=JZ(n)}   when has_repl n -> { x with opcode = JZ(repl n) }
                                         | {opcode=CALL(n)} when has_repl n -> { x with opcode = CALL(repl n)}
                                         | _                -> x
                                 )

    in let opt = remove_nops c |> rc |> opt_rev |> List.rev
(*     in let _ = print_endline "" ; print_endline "" *)
(*     in let _ = dump_code_lines opt *)
    in opt 

