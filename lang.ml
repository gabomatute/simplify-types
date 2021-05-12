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
  | Ls of ('t * 't exp list)
  | Nil of 't | Cons of 't exp * 't exp
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
  | Match of pattern

and pattern =
  | MLeft of formula
  | MRight of formula
  | MTuple of (name * formula) list

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

let rec bare = function
  | RVoid -> Void
  | RSum(l, r) -> Sum(bare l, bare r)
  | RProd ts -> Prod(List.map (fun (n, t) -> (n, bare t)) ts)
  | RLst t -> Lst(bare t)
  | Refine(t, phi) -> bare t


(* type synthesis and evaluation *)

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
  | Ls(t, es) ->
    if List.for_all (ssyn ~vars >> ((=) t)) es then Lst t else raise IllTyped
  | Nil t -> Lst t
  | Cons(e, es) ->
    let t = ssyn ~vars e in
    let t' = let Lst t = ssyn ~vars es in t in
    if t = t' then t else raise IllTyped
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

let rec eval ?(vars = []) e =
  try match e with
  | V n -> List.assoc n vars
  | L(e, t) -> L(eval ~vars e, t)
  | R(t, e) -> R(t, eval ~vars e)
  | Case(e, (ln, le), (rn, re)) ->
    begin match eval ~vars e with
      | L(e, t) -> eval ~vars:((ln, e) :: vars) le
      | R(t, e) -> eval ~vars:((rn, e) :: vars) re
      | _ -> raise IllTyped
    end
  | Tuple es ->
    Tuple(List.map (fun (n, e) -> (n, eval ~vars e)) es)
  | Proj(n, e) -> 
    let ts = let Tuple ts = eval ~vars e in ts in
    List.assoc n ts
  | Ls(t, es) ->
    Ls(t, List.map (eval ~vars) es)
  | Nil t -> Ls(t, [])
  | Cons(e, es) ->
    let e = eval ~vars e in
    let t, l = let Ls l = eval ~vars es in l in
    Ls(t, e :: l)
  | Map((n, f), e) ->
    let t, es = let Ls(t, es) = eval ~vars e in t, es in
    let ts = List.map (fun (n, e) -> (n, ssyn e)) vars in
    let es = List.map (fun e -> eval ~vars:((n, e) :: vars) f) es in
    Ls(ssyn ~vars:((n, t) :: ts) f, es)
  | Append(l, r) ->
    let t, ll = let Ls l = eval ~vars l in l in
    let t, rl = let Ls r = eval ~vars r in r in
    Ls(t, ll @ rl)
  | Flatten(n, e) ->
    let t, l = let Ls l = eval ~vars e in l in
    let t, i = let Some(t, i) = log t in t, i in
    Ls(t, List.concat_map begin fun e ->
        let es = let Tuple es = e in es in
        List.map snd es
      end l)
  with
  | Match_failure _ -> raise IllTyped
  | Not_found -> raise IllTyped


(* constraint checking *)

let rec ncompute e : number -> int =
  let rec len ?(e = e) = function
    | Dot(p, x) -> let Tuple es = e in len ~e:(List.assoc x es) p
    | Val -> let Ls(t, l) = e in List.length l in
  List.fold_left (fun acc (p, c) -> acc + c * len p) 0

let rec fcheck e = function
  | False -> false | True -> true
  | Or(l, r) -> fcheck e l || fcheck e r
  | LEq(l, r) -> ncompute e l <= ncompute e r
  | Match p -> pmatch e p

and pmatch e p = try match p with
  | MLeft phi -> let L(e, t) = e in fcheck e phi
  | MRight phi -> let R(t, e) = e in fcheck e phi
  | MTuple phis -> let es = let Tuple es = e in es in
    List.for_all (fun (n, phi) -> fcheck (List.assoc n es) phi) phis
  with
  | Match_failure _ -> false

let rec rcheck e = function
  | RVoid -> false
  | RSum(lt, rt) -> begin match e with
    | L(e, t) -> rcheck e lt
    | R(t, e) -> rcheck e rt
    | _ -> assert false
    end
  | RProd rts ->
    let es = let Tuple es = e in es in
    List.for_all (fun (n, rt) -> rcheck (List.assoc n es) rt) rts
  | RLst rt ->
    let l = let Ls(t, l) = e in l in
    List.for_all (fun e -> rcheck e rt) l
  | Refine(rt, formula) ->
    rcheck e rt && fcheck e formula