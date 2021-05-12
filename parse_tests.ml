open Utils
open Parse
open Unparse

let normalize =
  Str.global_replace (Str.regexp " ") ""
    >> Str.global_replace (Str.regexp "1len") "len"

let is_equivalent input output =
  String.equal
    (normalize input)
    (normalize output)

let parse_then_print input =
  print_endline ("Parsing '" ^ input ^ "'.");
  match parse refine input with
    | Ok r ->
        let output =
          srefine r
        in
        print_endline
          ("Parse success: " ^ output);
        print_endline
          ("Is equivalent? " ^ string_of_bool (is_equivalent input output))

    | Error msg ->
        print_endline ("!!! Parse failure: " ^ msg)

let test_parse_roundtrip input =
  parse refine input
  |> Result.map (srefine >> is_equivalent input)
  |> Result.value ~default:false

let parser_tests =
  [ "_|_"
  ; "<>"
  ; "<> + <> + [<>]"
  ; "<a:<>, b : <>, c : <>>"
  ; "<a:<>, b : <>, c : <>> + <>"
  ; "{ [< a : _|_ >] | T }"
  ; "{ <> | F }"
  ; "{ <> | T }"
  ; "{ <> | T V F}"
  ; "{ <> | T V F V F}"
  ; "{ <> | 4len val <= 3len val }"
  ; "{ <> | 4len val <= 3len val V T }"
  ; "{ <> | 2len val + 3len val <= 4len val }"
  ; "{ <> | 2len val <= 3len val + 4len val }"
  ; "{ <> | T V 1len val <= 1len val }"
  ; "{ <> | 1len val <= 1len val V T }"
  ; "{ <> | 4len val + 2len val.x.myPath <= 23len val.zzz V T }"
  ; "{ <> | match(Left T) }"
  ; "{ <> | match(Left match(Right F)) }"
  ; "{ <> | match(Left match(Right F)) }"
  ; "{ <> | match(<x ~ F, y ~ match(Right T)>) }"
  ; "{ <> | 1len val <= 1len val}"
  ; "{ <> | 4 + 1len val <= 1len val}"
  ; "{ <> | 1len val <= 2 + 1len val}"
  ; "{ <> | 8 + 1len val <= 2 + 1len val}"
  ; "{ <> | 8 + len val + 4len val <= 2 + 2len val + len val.zz}"
  ]

let () =
  List.iter parse_then_print parser_tests;
  print_endline @@
    "Pass all parser tests? "
      ^ string_of_bool (List.for_all test_parse_roundtrip parser_tests)
