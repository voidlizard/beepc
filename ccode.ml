open Std
open Printf
open ExtList

let clist = List.of_enum (Enum.mapi ( fun i x -> sprintf "%s0x%02X" (if i mod 8 == 0 then "\n" else "")
                                                                    (Char.code x) ) (input_chars stdin))

let _ = print_endline (String.concat ", " clist)

