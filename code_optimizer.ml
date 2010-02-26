open Printf
open ExtList
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

    in let update_jumps repl_tbl code =
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
 
    in let opt_fun_tails code =

        let lookup n epi1 genf =
            try
                let (nid,_) = List.assoc n epi1 in epi1, nid
            with Not_found ->
                let nid' = idgen() in (n, (nid', genf nid')) :: epi1, nid'

        in let epi_n n id =   {opcode=RET;   line_id=None;      comment=""}
                           :: {opcode=LTMP;  line_id=None;      comment=""}
                           :: {opcode=FS(n); line_id=None;      comment=""}
                           :: {opcode=STMP;  line_id=Some(id);  comment=(sprintf "func/%d epilogue" n)}
                           :: []

        in let rec opt_fun_tails_r code new_code epi1 jumps = match code with
        | {opcode=RET} :: {opcode=LTMP} :: {opcode=FS(n)} :: {opcode=STMP; line_id=Some(id); comment=c} :: xs -> 
            
            let epi1', nid = lookup n epi1 (fun id -> epi_n n id) 
            in opt_fun_tails_r xs ( {opcode=JMP(nid); line_id=Some(id); comment=c} :: new_code ) epi1' ((id,nid) :: jumps)

          (* It does not make much sense to replace void epilogue to jump *)
(*        | {opcode=RET} :: {opcode=FS(n)} :: xs                                   ->*)
(*            let epi1', nid = lookup n epi1 (fun id -> epi_n_void n id) *)
(*            in opt_fun_tails_r xs (new_code @ [{opcode=JMP(nid); line_id=Some(id); comment=""}] ) epi1' ((id,nid) :: jumps)*)

        | x :: xs                             -> opt_fun_tails_r xs (x :: new_code) epi1 jumps
        | []                                  -> (new_code, epi1, jumps)
        in let (new_code, epi1, jumps) = opt_fun_tails_r code [] [] []
        in let () = printf "REPL0 SIZE: %d\n" (List.length jumps)
        in (List.fold_left (fun acc (_,(_,c)) -> acc @ c) (List.rev new_code) epi1 ) |> update_jumps jumps 

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
        in let () = printf "REPL1 SIZE: %d\n" (List.length repl_tbl)
        in update_jumps repl_tbl new_code 

    in let opt = remove_nops c |> rc |> opt_rev |> opt_fun_tails |> List.rev
(*     in let _ = print_endline "" ; print_endline "" *)
(*     in let _ = dump_code_lines opt *)
    in opt 

