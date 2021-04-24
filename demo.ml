type const = int
type name = string

type simple = Void
  | Sum of simple * simple
  | Prod of (name * simple) list
  | Lst of simple

type exp = V of name
  | L of exp | R of exp
  | Case of exp * (name * exp) * (name * exp)
  | Tuple of (name * exp) list
  | Proj of (name * exp)
  | Nil | Cons of exp * exp | Append of exp * exp
  | Map of (name * exp) * exp | Flatten of int * exp

type path = Val
  | Dot of path * name

type number = N of int
  | Times of int * number
  | Plus of number * number
  | Len of path

type formula =
  | False | True
  | Or of formula * formula
  | LEq of number * number

type refine = RVoid
  | RSum of refine * refine
  | RProd of (name * refine) list
  | RLst of refine
  | Refine of refine * formula

let rec tselect t = function
  | Dot(p, x) -> let Prod ts = tselect t p in List.assoc x ts
  | Val -> t

let rec eselect e = function
  | Dot(p, x) -> let Tuple es = eselect e p in List.assoc x es
  | Val -> e

let rec slen = function
  | V "val" -> Len Val
  | Proj((x, e)) -> slen (eselect e (Dot(Val, x)))
  | Nil -> N 0 | Cons(e1, e2) -> Plus((N 1), (slen e2))
  | Append(e1, e2) -> Plus((slen e1), (slen e2))
  | Flatten(c, e) -> Times(c, slen e)
  | Map((x, e1), e2) -> slen e2
  | _ -> assert false

let rec nrewrite i = function
  | Times(c, n) -> Times(c, nrewrite i n)
  | Plus(n1, n2) -> Plus(nrewrite i n1, nrewrite i n2)
  | Len p -> slen (eselect (i (V "val")) p)
  | N c -> N c

let rec frewrite i = function
  | (False | True) as phi -> phi
  | Or(phi1, phi2) -> Or(frewrite i phi1, frewrite i phi2)
  | LEq(n1, n2) -> LEq(nrewrite i n1, nrewrite i n2)

type coeff = path option * int

let rearrange (LEq(l, r)) : coeff list * coeff list =
  let cmult k =
    List.map (fun (p, c) -> (p, k * c)) in
  let rec coeffs = function
    | Times(c, n) -> cmult c (coeffs n)
    | Plus(n1, n2) -> coeffs n1 @ coeffs n2
    | Len p -> [(Some p, 1)]
    | N c -> [(None, c)] in
  let extract p =
    List.partition_map begin function
      | pi, ci when pi = p -> Either.Left ci
      | pi, ci -> Either.Right (pi, ci)
    end in
  let rec collapse = function
    | (p, c)::rest -> let cs, remain = extract p rest in
      (p, List.fold_left (+) c cs) :: collapse remain
    | [] -> [] in
  let eqsplit =
    List.partition_map begin function
      | pi, ci when ci > 0 -> Either.Left (pi, ci)
      | pi, ci -> Either.Right (pi, -ci)
    end in
  eqsplit (collapse ((coeffs l) @ (cmult (-1) (coeffs r))))

let rec simplify = function
  | RSum(rt1, rt2) ->
    let i1, t1 = simplify rt1 in
    let i2, t2 = simplify rt2 in
    let i v = Case(v, ("x1", L(i1(V "x1"))), ("x2", R(i2(V "x2")))) in
    (i, Sum(t1, t2))
  | RProd rnts ->
    let ns, rts = List.split rnts in
    let is, ts = List.split (List.map simplify rts) in
    let i v = Tuple(List.map2 (fun n i -> (n, i(Proj(n, v)))) ns is) in
    (i, Prod(List.combine ns ts))
  | RLst rt ->
    let ielt, t = simplify rt in
    let i v = Map(("x", ielt(V "x")), v) in
    (i, Lst t)
  | Refine(t, Or(phi1, phi2)) ->
    let i1, t1 = simplify (Refine(t, phi1)) in
    let i2, t2 = simplify (Refine(t, phi2)) in
    let i v = Case(v, ("x1", i1(V "x1")), ("x2", i2(V "x2"))) in
    (i, Sum(t1, t2))
  | Refine (rt, LEq(n1, n2)) ->
    let i, t = simplify rt in
    let LEq(n1, n2) = frewrite i (LEq(n1, n2)) in
    let nu, nv = rearrange (LEq(n1, n2)) in
    (??)
  | Refine(_, False) | RVoid ->
    let i v = assert false in
    (i, Void)
  | Refine(rt, True) ->
    simplify rt

(**** to string helpers ****)

let snames so (x, o) = x ^ ": " ^ so o

let rec ssimple = function
  | Sum(l, r) -> ssimple l ^ " + " ^ ssimple r
  | Prod ts -> "< " ^ String.concat ", " (List.map (snames ssimple) ts) ^ " >"
  | Lst t -> "[" ^ ssimple t ^ "]"
  | Void -> "_|_"

let rec spath = function
  | Dot(p, x) -> spath p ^ "." ^ x
  | Val -> "val"

let rec snumber = function
  | N c -> string_of_int c
  | Times (c, n) -> string_of_int c ^ "(" ^ snumber n  ^ ")"
  | Plus (l, r) -> snumber l ^ " + " ^ snumber r
  | Len p -> "len " ^ spath p

let rec sformula = function
  | False -> "F" | True -> "T"
  | Or(l, r) -> sformula l ^ " V " ^ sformula r
  | LEq(l, r) -> snumber l ^ " <= " ^ snumber r

let rec srefine = function
  | RSum(l, r) -> srefine l ^ " + " ^ srefine r
  | RProd ts -> "< " ^ String.concat ", " (List.map (snames srefine) ts) ^ " >"
  | Refine(t, phi) -> "{ " ^ srefine t ^ " | " ^ sformula phi ^ " }"
  | RLst t -> "[" ^ srefine t ^ "]"
  | RVoid -> "F"

(**** demo ****)