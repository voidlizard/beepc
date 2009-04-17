open Printf
open Util
open ExtList
open ExtHashtbl

type beep_type = TModule | TVoid | TVoidCast of beep_type | TInt | TString | TBool
                         | TPair of beep_type * beep_type
                         | TList of beep_type
                         | TVect of beep_type
                         | TAtom
                         | TBlock
                         | TFun of beep_type list * beep_type 
                         | TRecField of beep_type * string 
                         | TVar of int
                         | TAny of int
                         | TRecord of (string  * beep_type) list
                         | TFunNative of int * beep_type list * beep_type
                         | TFunEmit of string * beep_type list * beep_type * Opcodes.opcode list
                         | TTypename of string
                         | TTypedef  of beep_type

type unify_errors = Circular | Unsolvable of beep_type * beep_type | Unsupported of beep_type 

exception Invalid_type
exception Unify_error of unify_errors

let raise_circular ()   = raise ( Unify_error(Circular))
let raise_unsolv t1 t2  = raise ( Unify_error(Unsolvable(t1,t2)) )
let raise_unsupp t      = raise ( Unify_error(Unsupported(t)) )

let str_of_tp x = 
    let rec s_atom = function
        | TVoid             -> "void"
        | TVoidCast(x)      -> sprintf "void_cast<%s>" (s_atom x)
        | TInt              -> "int"
        | TString           -> "string"
        | TAtom             -> sprintf "atom"
        | TBool             -> "bool"
        | TBlock            -> "block"
        | TVar x            -> sprintf "var<%d>" x
        | TAny x            -> sprintf "any<%d>" x
        | TPair(a,b)        -> sprintf "pair<%s, %s>" (s_atom a) (s_atom b)
        | TRecord(e)        -> sprintf "{%s}" (e |> List.map (fun (n,t) -> sprintf "%s:%s" n (s_atom t)) |> String.concat ",")
        | TList(a)          -> sprintf "list<%s>" (s_atom a)
        | TVect(a)          -> sprintf "vect<%s>" (s_atom a)
        | TFun(a,r)         -> sprintf "fun(%s):%s" (String.concat "," (List.map s_atom a)) (s_atom r)
        | TRecField(t,n)    -> sprintf "field (%s) (%s)" (s_atom t) n
        | TFunNative(n,a,r) -> sprintf "native fun_%d (%s):%s" n (String.concat "," (List.map s_atom a)) (s_atom r)
        | TModule           -> "module" 
        | TFunEmit(n,a,r,_) -> sprintf "fun emit %s (%s):%s" n (String.concat "," (List.map s_atom a)) (s_atom r)
        | TTypename(s)      -> sprintf "typename<%s>" s
        | TTypedef(t)       -> sprintf "typedef<%s>" (s_atom t)
    in s_atom x


let func_arity = function
    | TFun(l,r) | TFunNative(_,l,r) | TFunEmit(_,l,r,_) -> List.length l
    | x                                               -> 0

let func_ret_type = function
    | TFun(_,r) | TFunNative(_,_,r) | TFunEmit(_,_,r,_) -> r
    | x                                                 -> raise Invalid_type 

let rec_fields_num = function TRecord(f) -> List.length f | _ -> assert false

let field_rec_type = function TRecField(tp,name) -> tp | _ -> assert false

let rec_field_type n = function
    | TRecord(l) -> List.assoc n l
    | x          -> failwith (sprintf "NOT A RECORD %s" (str_of_tp x))

let rec with_atom_type f t = match t with
    | TPair(a, b)          -> TPair(with_atom_type f a, with_atom_type f b)
    | TList(a)             -> TList(with_atom_type f a)
    | TVect(a)             -> TVect(with_atom_type f a)
    | TRecord(l)           -> TRecord(List.map (fun (n,x) -> (n, with_atom_type f x)) l)
    | TRecField(t,n)       -> TRecField(with_atom_type f t, n)
    | TFun(a, r)           -> TFun(List.map (fun x-> with_atom_type f x) a, with_atom_type f r) 
    | TFunNative(n,a,r)    -> TFunNative(n, List.map (fun x-> with_atom_type f x) a, with_atom_type f r)
    | TFunEmit(n,a,r,c)    -> TFunEmit(n, List.map (fun x-> with_atom_type f x) a, with_atom_type f r, c)
    | x                    -> f x

let rec of_atom_type f t = match t with
    | TPair(a, b)       -> of_atom_type f a @ of_atom_type f b
    | TList(a)          -> of_atom_type f a
    | TVect(a)          -> of_atom_type f a
    | TRecord(l)        -> l |> List.map (fun (_,x) -> of_atom_type f x) |> List.flatten
    | TRecField(t,_)    -> of_atom_type f t
    | TFun(a, r)
    | TFunNative(_,a,r)
    | TFunEmit(_, a,r,_)-> of_atom_type f r @ a |> List.map (fun x -> of_atom_type f x) |> List.flatten
    | x                 -> f x

let poly_replaces t = 
    let atoms = of_atom_type (function TAny(x) -> [TAny(x)] | _ -> [] ) t |> List.unique
    in atoms |> List.map (fun x -> (x, TAny(Parser_ctx.uniq_id())))

let t_repl dict t = 
    let repl f = try List.assoc f dict with Not_found -> f
    in with_atom_type repl t

let unify constr = 
    let rec uni l =
        let rec sub_step x y a = match a with
        | TVar(s) -> if s = x then y else TVar(s)
        | TAny(s) -> if s = x then y else TAny(s)
        | TInt    -> TInt
        | TBool   -> TBool
        | TString -> TString
        | TAtom   -> TAtom
        | TVoid   -> TVoid
        | TPair(a,b) -> TPair(sub_step x y a, sub_step x y b) 
        | TList(a)   -> TList(sub_step x y a)
        | TVect(a)   -> TVect(sub_step x y a)
        | TRecord(l) -> TRecord(List.map (fun (n,t) -> (n,sub_step x y t)) l)
        | TRecField(t,n) -> TRecField(sub_step x y t, n)
        | TFun(a,r)  -> TFun(List.map (fun t -> sub_step x y t) a, sub_step x y r)
        | TFunEmit(n,a,r,op) -> TFunEmit(n,List.map (fun t -> sub_step x y t) a, sub_step x y r, op)
        | TFunNative(n,a,r) -> TFunNative(n, List.map (fun t -> sub_step x y t) a, sub_step x y r)
        | x          -> raise_unsupp x

        in let sub x y rs = List.map (fun (a,b) -> ((sub_step x y a), (sub_step x y b)) ) rs

        in let occurs x t = 
            let rec check t = match t with
            | TVar(s)      -> (s = x)
            | TAny(s)      -> false
            | TPair(a, b)  -> check a || check b
            | TList(a)     -> check a
            | TVect(a)     -> check a
            | TFunEmit(_,p,r,_)
            | TFunNative(_,p,r)
            | TFun(p,r)    -> check_in p || check r
            | TRecField(t,_) -> check t
            | TTypename(s) -> false
            | TTypedef(a)  -> check a
            | TRecord(e)   -> e |> List.map (fun (_,a) -> check a) 
                               |> List.fold_left (fun l r -> l = true || r = true ) false
            | TInt | TBool | TString | TVoid | TVoidCast _ | TBlock | TModule | TAtom -> false
            and check_in l = match l with
                | x::xs -> check x || check_in xs
                | []    -> false

            in if check t then let _ = printf "JOPA %d in %s\n" x (str_of_tp t) in raise_circular () else ()

        (* TODO: USE unified field enumeration algorithm here! *)
        in let with_records x y = match (x,y) with
            |(TRecord(r1),TRecord(r2)) ->
                let cmp = fun (s1,_) (s2,_) -> compare s1 s2
                in let r1 = List.sort r1 ~cmp:cmp
                in let r2 = List.sort r2 ~cmp:cmp
                in let r3 = List.map2 (fun x y -> (x,y)) r1 r2 (* TODO: different sizes *)
                in let merge (a,b) = 
                    if fst a = fst b then (snd a, snd b) else raise_unsolv x y
                in List.map merge r3
            | _ -> assert false

(*         List.map2 (fun (_,x) (_,y) -> (x,y)) *)

        in match l with
        | []                        -> []
        | (TVar(x), y)::rest        -> occurs x y;
                                       if TVar(x) = y then uni rest else uni (sub x y rest) @ [(TVar(x), y)]
        | (TAny(x), y)::rest        -> occurs x y;
                                       if TAny(x) = y then uni rest else uni (sub x y rest) @ [(TAny(x), y)]
        | (y, TVar(x))::rest        -> uni ((TVar(x),y)::rest)
        | (y, TAny(x))::rest        -> uni ((TAny(x),y)::rest)

        | (TInt, TInt)::rest        -> uni rest
        | (TBool, TBool)::rest      -> uni rest
        | (TVoid, TVoid)::rest      -> uni rest
        | (TString, TString)::rest  -> uni rest
        | (TAtom, TAtom)::rest      -> uni rest
        | (TPair(a,b), TPair(a',b'))::rest -> uni ((a,a') :: (b,b') :: rest)
        | (TList(a), TList(b))::rest -> uni ( (a,b) :: rest )
        | (TVect(a), TVect(b))::rest -> uni ( (a,b) :: rest )
        | (TRecord(a), TRecord(b))::rest  -> uni ( (with_records (TRecord(a)) (TRecord(b))) @ rest )
        | (TRecField(a1,n),TRecField(a2,n2))::rest -> uni ((a1, a2)::rest)
        | (TFun(a,r), TFun(a1, r1))::rest -> uni ((r,r1) :: List.map2 (fun x y -> (x,y)) a a1 @ rest )
        | (TFunEmit(n,a,r,_), TFunEmit(n1,a1, r1,_))::rest when n = n1 -> uni ((r,r1) :: List.map2 (fun x y -> (x,y)) a a1 @ rest )
        | (TFunNative(_,a,r), TFunNative(_,a1, r1))::rest -> uni ((r,r1) :: List.map2 (fun x y -> (x,y)) a a1 @ rest )
        | (x, y)::rest -> printf "FUCKUP: %s  <-> %s\n" (str_of_tp x) (str_of_tp y)  ; raise_unsolv x y
    in uni constr |> uni

