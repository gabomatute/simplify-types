open Lang
open Simplify
open Enumerate
open Parse
open Unparse

let n = 10
let () =
  print_endline ("List unroll factor: " ^ string_of_int n);
  print_newline ()

exception Counter of simple exp * simple exp
let validate rt i t =
  einit ~n t begin fun e ->
    let out = eval (i e) in
    if rcheck out rt then () else
    raise (Counter(e, out))
  end

let showcase input =
  print_endline ("refine = " ^ input ^ "");
  match parse refine input with
    | Ok rt ->
        let i, t = simplify rt in
        print_endline ("simple = " ^ ssimple t);
        let ival = optimize ~v:("val", t) (i (V "val")) in
        print_endline ("i(val) = " ^ sexp ssimple ival);
        print_endline ("validating...");
        begin try
          validate rt i t;
          print_endline "= OK"
        with
          | Counter(e, out) ->
            print_endline "!!! FAILURE";
            print_endline ("input = " ^ sexp ssimple e);
            print_endline ("out = " ^ sexp ssimple out);
        end;
        print_newline ()
    | Error msg ->
        print_endline ("!!! Parse failure: " ^ msg)

let examples =
  [ "<>"
  ; "<> + <>"
  ; "[<>]"
  ; "<a:<>>"
  ; "<a:<> + <>>"
  ; "<a:[<>]>"
  ; "{ <> | T }"
  ; "<a:[<>], b:[<>]>"
  ; "{ <a:[<>]> | 0len val.a <= 1len val.a }"
  ; "{ <a:[<>]> | 1len val.a <= 0len val.a}"
  ; "{ <a:[<>], b:[<>]> | 1len val.a <= 1len val.b}"
  ; "{ <a:[<>], b:[<>]> | 2len val.a <= 3len val.b}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 1len val.a + 1len val.b <= 1len val.c}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 1len val.a <= 1len val.b + 1len val.c}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 2len val.a + 3len val.b <= 5len val.c}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 5len val.a <= 2len val.b + 3len val.c}"
  ; "{ { <a:[<>], b:[<>]> | 1len val.a <= 1len val.b} | 1len val.b <= 1len val.a}"
  ; "{ <a:[<>], b:[<>], c:[<>]> | 1len val.a <= 1len val.b V 1len val.a <= 1len val.c }"
  ]

let () =
  List.iter showcase examples