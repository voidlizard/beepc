open Printf
open Opcodes
open Code
open Util

let optimize c =
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
    in let rc = List.rev c
(*     in let _ = dump_code_lines rc *)
    in let opt = rc |> opt_rev |> List.rev
(*     in let _ = print_endline "" ; print_endline "" *)
(*     in let _ = dump_code_lines opt *)
    in opt 

