open Parsing

let uniq_id = let d = ref 0 in function () -> d := 1 + !d ; !d;;

type parser_ctx = { pos : Lexing.position; uid : int }

let ctx  = fun _ -> { pos = Parsing.rhs_start_pos 1; uid = uniq_id () }

