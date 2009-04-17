open Printf
open ExtString

exception Not_opcode

let defines = function
    | (s, Some n) -> printf "#define %-8s 0x%02X\n" s n
    | (s, None)   -> failwith (sprintf "OPCODE %s NOT DEFINED" s)

let getn = function
        | (_, None)    -> 0
        | (_, Some(x)) -> x

let geto (x,_) = x 

let generate_array w ll =
    let last = getn( List.hd ( List.rev ll ) )
    in let opcodes = Array.make (last+1) ("NOP", None)
    in List.iter  ( fun x   -> opcodes.(getn x) <- x ) ll;
      Array.iteri ( fun i x -> printf "%s&&L_%-8s%s" (if i>0 then "," else " ")
                                                     (geto x) 
                                                     (if (i+1) mod w <> 0 then "" else "\n") ) opcodes
let generate_skel ll = 
    let ident = "    " 
    in List.iter (fun x -> printf "%sOP(%s) {\n%sNEXT();\n%s}\n" ident (geto x) (String.concat "" [ident;ident]) ident) ll


let gen_array () = generate_array 8 Opcodes.opcodetbl
let gen_skel ()  = generate_skel Opcodes.opcodetbl
let gen_enum ()  = List.iter defines Opcodes.opcodetbl


