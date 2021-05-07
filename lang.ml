type const = int
type name = string

type simple = Void
  | Sum of simple * simple
  | Prod of (name * simple) list
  | Lst of simple

type 't exp = V of name
  | L of 't exp * 't | R of 't * 't exp
  | Case of 't exp * (name * 't exp) * (name * 't exp)
  | Tuple of (name * 't exp) list | Proj of (name * 't exp)
  | Append of 't exp * 't exp | Flatten of int * 't exp
  | Map of (name * 't exp) * 't exp

type path = Val
  | Dot of path * name

type number =
  (path * int) list

type formula =
  | False | True
  | Or of formula * formula
  | LEq of number * number

type refine = RVoid
  | RSum of refine * refine
  | RProd of (name * refine) list
  | RLst of refine
  | Refine of refine * formula


(* number pseudo-constructors *)

open Utils

let len p : number =
  [(p, 1)]

let add (n: number) : number -> number =
  List.fold_left (fun n (p, c) -> assoc_update ((+) c) p ~v:0 n) n

let mult c : number -> number =
  List.map (fun (p, ci) -> (p, c * ci))


(* type operators *)

let power t n =
  Prod(List.init n (fun i -> (string_of_int (i + 1), t)))

let rec log (Prod ts) = match ts with
  | [] -> Some(Prod [], 0)
  | (_, t) :: [] -> Some(t, 1)
  | (_, t') :: ts ->
    let* t, i = log (Prod ts) in
    if t' = t then Some(t, i + 1) else None


(* type synthesis *)

exception IllTyped
let rec ssyn ?(vars = []) e =
  try match e with
  | V n -> List.assoc n vars
  | L(e, t) -> Sum(ssyn ~vars e, t)
  | R(t, e) -> Sum(t, ssyn ~vars e)
  | Case(e, (ln, le), (rn, re)) ->
    let lt, rt = let Sum(l, r) = ssyn ~vars e in l, r in
    let lt = ssyn ~vars:((ln, lt) :: vars) le in
    let rt = ssyn ~vars:((rn, rt) :: vars) re in
    if lt = rt then lt else raise IllTyped
  | Tuple es ->
    Prod(List.map (fun (n, e) -> (n, ssyn ~vars e)) es)
  | Proj(n, e) -> 
    let ts = let Prod ts = ssyn ~vars e in ts in
    List.assoc n ts
  | Map((n, f), e) ->
    let t = let Lst t = ssyn ~vars e in t in
    Lst(ssyn ~vars:((n, t) :: vars) f)
  | Append(l, r) ->
    let lt, rt = let Lst l, Lst r = ssyn ~vars l, ssyn ~vars r in l, r in
    if lt = rt then Lst lt else raise IllTyped
  | Flatten(n, e) ->
    let t = let Lst t = ssyn ~vars e in t in
    let t, i = let Some(t, i) = log t in t, i in
    if i = n then Lst t else raise IllTyped
  with
  | Match_failure _ -> raise IllTyped
  | Not_found -> raise IllTyped