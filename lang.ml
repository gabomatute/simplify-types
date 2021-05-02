type const = int
type name = string

type simple = Void
  | Sum of simple * simple
  | Prod of (name * simple) list
  | Lst of simple

type exp = V of name
  | L of exp | R of exp
  | Case of exp * (name * exp) * (name * exp)
  | Tuple of (name * exp) list | Proj of (name * exp)
  | Append of exp * exp | Flatten of int * exp
  | Map of (name * exp) * exp

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