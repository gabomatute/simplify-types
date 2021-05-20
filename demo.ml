open Lang
open Simplify
open Enumerate
open Parse
open Unparse


(* Enumeration settings *)

let setting name v =
  print_endline (name ^ ": " ^ string_of_int v); v

let einit = einit
  ~lmax:(setting "List unroll factor" 3)

let rinit () = rinit
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
  ]

(* Helpers *)

let v : name = "val"

let simplify rt =
  let i, t = simplify rt in
  let iopt (V n) = optimize ~v:(n, t) (i (V n)) in
  (iopt (V v), t)

exception Counter of simple exp * simple exp
let validate rt ival t =
  Seq.iter begin fun e ->
    let out = eval ~vars:[v, e] ival in
    if rcheck out rt then () else
    raise (Counter(e, out))
  end (einit t)

let showcounter input output =
  print_endline ("input  = " ^ sexp ssimple input);
  print_endline ("output = " ^ sexp ssimple output)

let showcase input =
  match parse refine input with
  | Ok rt ->
    print_newline ();
    print_endline ("refine = " ^ srefine rt ^ "");
    let ival, t = simplify rt in
    print_endline ("simple = " ^ ssimple t);
    print_endline ("i(val) = " ^ sexp ssimple ival);
    print_string "validating..."; flush stdout;
    begin try
      validate rt ival t;
      print_endline " OK"
    with
      | Counter(input, output) ->
        print_endline " counterexample";
        showcounter input output;
        print_endline "!!! FAILURE";
    end
  | Error msg ->
    print_endline ("!!! Parse failure: " ^ msg)

let c = ref 0
let crunch rt =
  if (incr c; !c) mod 1000000 = 0 then
    print_string "."; flush stdout;
  let ival, t = simplify rt in
  try validate rt ival t with
  | Counter(i, o) ->
    showcounter i o;
    assert false

let repl () =
  let read () =
    read_line (print_string "parse  > ") in
  try while true do showcase (read ()) done with
    | End_of_file -> ()

let () = match Sys.argv with
  | [| prog; "-i" |] -> repl ()
  | [| prog; "-e" |] -> Seq.iter crunch (rinit ())
  | [| prog |] -> List.iter showcase examples
  | _ -> let prog = Sys.argv.(0) in
    print_endline ("help: " ^ prog ^ "[ -i | -e ]")
