open Printf
open ExtList
open Util

type codeid = int

type opcode =
    | EMIT of opcode list
    | DATA of int
    | ADDROF of int
    | NOP
    | ALLOC
    | CONS
    | GC
    | CGC
    | RW 
    | WW              (* ADDR W -- ADDR+1 *)
    | TBADDR          (* OFFSET ADDR -- ADDR+OFFSET*SIZE *)
    | TBCR  of int    (* TABLE BASE CONST READ:   ADDR -- VAL ; READ  WORD FROM ADDR+LIT, DROP ADDR *)    
    | TBCW  of int    (* TABLE BASE CONST WRITE:  VAL ADDR -- ADDR ; WRITE WORD TO ADDR+LIT, _LEAVES_ ADDR *)
    | TBCWD of int    (* TABLE BASE CONST WRITE:  VAL ADDR -- ADDR ; WRITE WORD TO ADDR+LIT, _DROPS_ ADDR *)
    | LW of int       (* LOADS LITERAL TO ADDR (TOP), INCREMENTS STACK TOP *)
    | SF of int       (* SET FRAME = FRAME -> RSTACK; FRAME <- STACK_TOP - X *)
    | FS of int       (* SET FRAME = RSTACK -> FRAME; TOP = FRAME - X *)
    | NDUP of int     (* RESERVES N WORDS OF STACK *)
    | STMP
    | LTMP
    | RET
    | JMP of codeid 
    | JNZ of codeid
    | JZ  of codeid
    | CALL of codeid
    | NCALL of int
    | CALLT          (* CALL TOP STACK; DROP *)
    | NCALLT         (* NCALL TOP STACK; DROP *)
    | NEG
    | INC
    | DEC
    | INCA           (* increment address, += 2 *)
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | AND
    | OR
    | XOR
    | INV
    | SHL
    | SHR
    | LE                   (* 1 IF LESS *)
    | GT                   (* 1 IF GT   *)
    | EQ                   (* 1 IF EQ   *)
    | NEQ                  (* 0 IF EQ   *)
    | LEQ                  (* 1 OF LESS OR EQUAL *)
    | GEQ                  (* 1 OF GREATER OR EQUAL *)
    | NN                   (* 1 IF != 0 ELSE 0 *)
    | NOT                  (* !0 -> 0, 0 -> 1 *)
    | DUP
    | DROP 
    | TRUE                 (* LIT 0001 *)
    | FALSE                (* LIT 0000 *)
    | LIT of int
    | LOADF  of int
    | STOREF of int
    | LOAD0
    | LOAD1
    | LOAD2
    | LOAD3
    | LOAD4
    | LOAD5
    | LOAD6
    | LOAD7
    | STORE0
    | STORE1
    | STORE2
    | STORE3
    | STORE4
    | STORE5
    | STORE6
    | STORE7
    | DUMP 
    | DOWN

let has_literal = function 
    | EMIT _
    | DATA _
    | ADDROF _
    | LW _
    | TBCW _ | TBCWD _ | TBCR _
    | SF _
    | FS _
    | NDUP _
    | JMP _
    | JNZ _
    | JZ _
    | CALL _
    | NCALL _
    | LIT _
    | LOADF _
    | STOREF _ -> true
    | _        -> false

let get_literal = function 
    | SF x | FS x | LOADF x | STOREF x | LIT x | TBCR x | TBCW x | TBCWD x
    | NCALL x | NDUP x                  -> x
    | _                                 -> failwith "OPCODE W/O LITERAL"

let has_addr = function
    | JMP(_) | JNZ(_) | JZ(_) | CALL(_) -> true
    | _                                 -> false

let get_addr = function
    | JMP(x) | JNZ(x) | JZ(x) | CALL(x) -> x
    | _                                 -> failwith "OPCODE W/O ADDR"

let str_of_opcode = function
    | EMIT _   ->  "EMIT"
    | DATA _   ->  "DATA"
    | ADDROF _ ->  "ADDROF"
    | NOP      ->  "NOP"
    | ALLOC    ->  "ALLOC"
    | CONS     ->  "CONS"
    | GC       ->  "GC"
    | CGC      ->  "CGC"
    | RW       ->  "RW" 
    | WW       ->  "WW"
    | LW   x   ->  "LW"
    | TBADDR   ->  "TBADDR"
    | TBCR x   ->  "TBCR"
    | TBCW x   ->  "TBCW"
    | TBCWD x  ->  "TBCWD"
    | SF   x   ->  "SF"
    | FS   x   ->  "FS"
    | NDUP x   ->  "NDUP"
    | STMP     ->  "STMP"
    | LTMP     ->  "LTMP"
    | RET      ->  "RET"
    | JMP  x   ->  "JMP" 
    | JNZ  x   ->  "JNZ" 
    | JZ   x   ->  "JZ"
    | CALL x   ->  "CALL"
    | NCALL x  ->  "NCALL"

    | CALLT    ->  "CALLT"
    | NCALLT   ->  "NCALLT"

    | NEG      ->  "NEG"
    | INCA     ->  "INCA"
    | INC      ->  "INC"
    | DEC      ->  "DEC"
    | ADD      ->  "ADD"
    | SUB      ->  "SUB"
    | MUL      ->  "MUL"
    | DIV      ->  "DIV"
    | MOD      ->  "MOD"
    | AND      ->  "AND"
    | OR       ->  "OR"
    | XOR      ->  "XOR"
    | INV      ->  "INV"
    | SHL      ->  "SHL"
    | SHR      ->  "SHR"
    | NN       ->  "NN"
    | NOT      ->  "NOT"
    | EQ       ->  "EQ"
    | NEQ      ->  "NEQ"
    | LE       ->  "LE"
    | GT       ->  "GT"
    | LEQ      ->  "LEQ"
    | GEQ      ->  "GEQ"
    | DUP      ->  "DUP"
    | DROP     ->  "DROP"
    | LIT    _ ->  "LIT"
    | TRUE     ->  "TRUE"
    | FALSE    ->  "FALSE"
    | LOADF  _ ->  "LOADF"
    | STOREF _ ->  "STOREF"
    | LOAD0    ->  "LOAD0"
    | LOAD1    ->  "LOAD1"
    | LOAD2    ->  "LOAD2"
    | LOAD3    ->  "LOAD3"
    | LOAD4    ->  "LOAD4"
    | LOAD5    ->  "LOAD5"
    | LOAD6    ->  "LOAD6"
    | LOAD7    ->  "LOAD7"
    | STORE0   ->  "STORE0"
    | STORE1   ->  "STORE1"
    | STORE2   ->  "STORE2"
    | STORE3   ->  "STORE3"
    | STORE4   ->  "STORE4"
    | STORE5   ->  "STORE5"
    | STORE6   ->  "STORE6"
    | STORE7   ->  "STORE7"
    | DUMP     ->  "DUMP"
    | DOWN     ->  "DOWN"

let str_of_lit = function
    | EMIT   x -> sprintf "[...]"
    | DATA   x  
    | ADDROF x  
    | SF     x  
    | FS     x  
    | TBCW   x
    | TBCWD  x
    | TBCR   x
    | LIT    x  
    | LOADF  x  
    | STOREF x  
    | JMP    x  
    | JNZ    x  
    | JZ     x  
    | CALL   x  
    | NCALL  x  
    | LW     x  
    | NDUP   x -> sprintf "%04X" x
    | _        -> ""

let entry ?code:(c=None) s  = (s, c)

let code c = (Some c)

let opcodetbl = 
    let _tbl = [
    entry "NOP" ~code:(code 0x00);
    entry "ALLOC";
    entry "CONS";
    entry "GC";
    entry "CGC";
    entry "RW" ;
    entry "WW";
    entry "LW";
    entry "TBADDR";
    entry "TBCR";
    entry "TBCW";
    entry "TBCWD";
    entry "SF";
    entry "FS";
    entry "NDUP";
    entry "STMP";
    entry "LTMP";
    entry "RET";
    entry "JMP" ;
    entry "JNZ" ;
    entry "JZ";
    entry "CALL";
    entry "NCALL";
    entry "CALLT";
    entry "NCALLT";
    entry "NEG";
    entry "INCA";
    entry "INC";
    entry "DEC";
    entry "ADD";
    entry "SUB";
    entry "MUL";
    entry "DIV";
    entry "MOD";
    entry "AND";
    entry "OR";
    entry "XOR";
    entry "INV";
    entry "SHL";
    entry "SHR";
    entry "NN";
    entry "NOT";
    entry "EQ";
    entry "NEQ";
    entry "LE";
    entry "GT";
    entry "LEQ";
    entry "GEQ";
    entry "DUP";
    entry "DROP";
    entry "LIT";
    entry "TRUE";
    entry "FALSE";
    entry "LOADF";
    entry "STOREF";
    entry "LOAD0";
    entry "LOAD1";
    entry "LOAD2";
    entry "LOAD3";
    entry "LOAD4";
    entry "LOAD5";
    entry "LOAD6";
    entry "LOAD7";
    entry "STORE0";
    entry "STORE1";
    entry "STORE2";
    entry "STORE3";
    entry "STORE4";
    entry "STORE5";
    entry "STORE6";
    entry "STORE7";
    entry "DUMP"  ~code:(code 0xFE);
    entry "DOWN"  ~code:(code 0xFF);
   ]
   in let valid s = if List.length (List.unique s ~cmp:(fun (s,c) (s1,c1) -> c = c1 )) = (List.length s)
                    then s
                    else failwith "INVALID OPCODES LIST "
   in _tbl |> List.mapi (fun i (s,n) -> (s,match n with None -> Some(i) | Some(k) -> Some(k))) |> valid


let code c = match List.assoc (str_of_opcode c) opcodetbl with 
             | Some(x) -> x
             | None    -> raise Not_found

