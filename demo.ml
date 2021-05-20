open Utils
open Lang
open Simplify
open Enumerate
open Parse
open Unparse


(* Enumeration settings *)

let setting name v =
  print_endline (name ^ ": " ^ string_of_int v); v

let eiter = eiter
  ~lmax:(setting "List unroll factor" 3)

let riter () = riter
  ~dmax:(setting "Max type depth" 3)
  ~pmax:(setting "Max record size" 2)
  ~fmax:(setting "Max # constraints" 2)
  ~tmax:(setting "Max lens per term" 2)
  ~cmax:(setting "Max contraint constant" 2)


(* Examples *)

let examples =
  [ "<>"
  ; "<> + <>"
  ; "[<>]"
  ; "<a:<>>"
  ; "<a:<> + <>>"
  ; "<a:[<>]>"
  ; "{ <> | T }"
  ; "<a:[<>], b:[<>]>"
  ; "{ <a:[<>]> | 0 <= len val.a }"
  ; "{ <a:[<>]> | len val.a <= 0 }"
  ; "{ <a:[<>], b:[<>]> | len val.a <= len val.b}"
  ; "{ <a:[<>], b:[<>]> | 2len val.a <= 3len val.b}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | len val.a + len val.b <= len val.c}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | len val.a <= len val.b + len val.c}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 2len val.a + 3len val.b <= 5len val.c}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 5len val.a <= 2len val.b + 3len val.c}"
  ; "{ { <a:[<>], b:[<>]> | len val.a <= len val.b} | len val.b <= len val.a}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | len val.a <= len val.b V len val.a <= len val.c }"
  ; "{ <> + <> | match(Left T) }"
  ; "{ <> + <> | match(Right T) }"
  ; "{ <a: <>, b: <>> | match(<a ~ T, b ~ T>) }"
  ; "{ <> + <> | match(Left T) V match(Right T) }"
  ]

(* Helpers *)

let show msg ?rt ?t ?iv ?input ?output () =
  begin match msg with
    | Ok Some msg -> print_endline msg | Ok None -> ()
    | Error msg -> print_endline ("!!! " ^ msg) end;
  Option.iter (srefine >> (^) "refine = " >> print_endline) rt;
  Option.iter (ssimple >> (^) "simple = " >> print_endline) t;
  Option.iter (sexp ssimple >> (^) "i(val) = " >> print_endline) iv;
  Option.iter (sexp ssimple >> (^) "input  = " >> print_endline) input;
  Option.iter (sexp ssimple >> (^) "output = " >> print_endline) output

let v : name = "val"

let simplify rt =
  let i, t = try simplify rt with err ->
    show (Error "Simplify exception") ~rt (); raise err in
  let i (V n) = try optimize ~v:(n, t) (i (V n)) with err ->
    show (Error "Optimize exception") ~rt ~t ~iv:(i (V "val")) (); raise err in
  (i (V v), t)

exception Counter of simple exp * simple exp
let validate rt iv t = try
    Seq.iter begin fun input ->
      let output = try eval ~vars:[v, input] iv with err ->
        show (Error "Evaluation error") ~rt ~iv ~input (); raise err in
      if try not (rcheck output rt) with err ->
        show (Error "Check exception") ~rt ~output (); raise err then
      raise (Counter(input, output))
    end (eiter t)
  with
    | Counter(input, output) as err ->
      show (Error "Counter example") ~rt ~t ~iv ~input ~output ();
      raise err

let showcase input =
  match parse refine input with
  | Ok rt ->
    let iv, t = simplify rt in
    show (Ok None) ~rt ~t ~iv ();
    validate rt iv t;
    print_endline "Validated!";
    print_newline ()
  | Error msg ->
    show (Error("Parse failure: " ^ msg)) ()

let c = ref 0
let crunch rt =
  let iv, t = simplify rt in
  if (incr c; !c) mod 1000000 = 0 then
    show (Ok(Some(string_of_int !c))) ~rt ~t ~iv ();
  validate rt iv t

let repl () =
  let read () =
    read_line (print_string "parse  > ") in
  try while true do showcase (read ()) done with
    | End_of_file -> ()

let () = match Sys.argv with
  | [| prog; "-i" |] -> repl ()
  | [| prog; "-e" |] -> Seq.iter crunch (riter ())
  | [| prog |] -> List.iter showcase examples
  | _ -> let prog = Sys.argv.(0) in
    print_endline ("help: " ^ prog ^ "[ -i | -e ]")
