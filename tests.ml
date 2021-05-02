open Unparse

module Parser = struct
  let remove_spaces =
    Str.global_replace (Str.regexp " ") ""

  let is_equivalent input output =
    String.equal
      (remove_spaces input)
      (remove_spaces output)

  let parse_then_print input =
    print_endline ("Parsing '" ^ input ^ "'.");
    match Bark.run Parse.refine input with
      | Ok r ->
          let output =
            srefine r
          in
          print_endline
            ("Parse success: " ^ output);
          print_endline
            ("Is equivalent? " ^ string_of_bool (is_equivalent input output))

      | Error dead_ends ->
          let show_dead_end =
            fun dead_end ->
              match dead_end.Bark.problem with
                | Parse.Expecting s ->
                    "expecting '" ^ s ^ "'"
          in
          let dead_ends_string =
            dead_ends
              |> List.map show_dead_end
              |> String.concat ", "
          in
          print_endline ("!!! Parse failure: " ^ dead_ends_string)

  let test_parse_roundtrip input =
    match Bark.run Parse.refine input with
      | Ok parsed_type ->
          parsed_type
            |> srefine
            |> is_equivalent input

      | Error _ ->
          false

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
    ]

  let run () =
    List.iter parse_then_print parser_tests;
    print_endline @@
      "Pass all parser tests? "
        ^ string_of_bool (List.for_all test_parse_roundtrip parser_tests)
end

let () =
  Parser.run()