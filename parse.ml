open Utils
open Lang

open Bark
open Bark.Syntax

(* === Parser specialization === *)

type problem =
  | Expecting of string

type context =
  string

type 'a parser =
  (context, problem, 'a) Bark.parser

(* === Parser helpers === *)

let chainl1 : 'a 'b .
 context -> 'a parser -> 'b parser -> ('a -> 'b -> 'a) parser -> 'a parser =
  fun chain_context p_head p_arg op ->
    let rec next acc =
      one_of
        [ in_context chain_context
            ( let* combiner =
                op
              in
              p_arg |> and_then (combiner acc >> next)
            )
        ; succeed acc
        ]
    in
    p_head |> and_then next

let chainr1 : 'a .
 context -> 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser =
  fun chain_context p op ->
    let rec rest acc =
      one_of
        [ in_context chain_context
            ( let* combiner =
                op
              in
              map (combiner acc) (p |> and_then rest)
            )
        ; succeed acc
        ]
    in
    p |> (and_then rest)

let ignore_with : 'a -> unit parser -> 'a parser =
  fun x p ->
    map (fun _ -> x) p

let make_token : string -> problem token =
  fun tok ->
    Token (tok, Expecting tok)

let space_symbol =
  make_token " "

let spaces1 : unit parser =
  succeed ()
    |. symbol space_symbol
    |. spaces

(* === Symbols and Keywords === *)

(* Paths and numbers *)

let val_keyword =
  make_token "val"

let path_sep_symbol =
  make_token "."

let len_keyword =
  make_token "len"

let plus_symbol =
  make_token "+"

(* Patterns *)

let left_keyword =
  make_token "Left"

let right_keyword =
  make_token "Right"

let prod_constrains_symbol =
  make_token "~"

let nil_symbol =
  make_token "[]"

let cons_symbol =
  make_token "::"

(* Formulae *)

let false_keyword =
  make_token "F"

let true_keyword =
  make_token "T"

let match_keyword =
  make_token "match"

let lparen_symbol =
  make_token "("

let rparen_symbol =
  make_token ")"

let leq_symbol =
  make_token "<="

let or_symbol =
  make_token "V"

(* Types *)

let void_symbol =
  make_token "_|_"

let sum_symbol =
  make_token "+"

let list_left_symbol =
  make_token "["

let list_right_symbol =
  make_token "]"

let prod_left_symbol =
  make_token "<"

let prod_sep_symbol =
  make_token ","

let prod_ascription_symbol =
  make_token ":"

let prod_right_symbol =
  make_token ">"

let refine_left_symbol =
  make_token "{"

let refine_mid_symbol =
  make_token "|"

let refine_right_symbol =
  make_token "}"

(* === Parsing === *)

(* Paths *)

let inner_char : char -> bool =
  fun c ->
    Utils.lowercase_char c
      || Utils.uppercase_char c
      || Utils.digit_char c
      || Char.equal c '_'

let reserved_words =
  String_set.of_list []

let variable_name : string parser =
  variable
    ~start:Utils.lowercase_char
    ~inner:inner_char
    ~reserved:reserved_words
    ~expecting:(Expecting "variable name")

let path : path parser =
  succeed (fun p -> p)
    |= chainl1 "path"
		     ( ignore_with Val (keyword val_keyword)
		     )
		     variable_name
         ( ignore_with (fun base ext -> Dot (base, ext))
             ( succeed ()
                 |. symbol path_sep_symbol
                 |. spaces
             )
         )
    |. spaces

(* Numbers *)

let scaled_length : (path * int) parser =
	succeed (fun n p -> (p, n))
		|= int (Expecting "scalar")
		|. keyword len_keyword
		|. spaces1
    |= path

let number : number parser =
  let affine : int parser =
    succeed (fun a -> a)
      |= int (Expecting "affine constant")
      |. spaces
      |. symbol plus_symbol
      |. spaces
  in
  let linear : (path * int) list parser =
    map List.rev @@
      chainl1 "number"
        ( succeed (fun n -> [n])
            |= scaled_length
        )
        scaled_length
        ( ignore_with (fun ns n -> n :: ns)
            ( succeed ()
                |. symbol plus_symbol
                |. spaces
            )
        )
  in
  one_of
    [ backtrackable
        ( succeed (fun affine linear -> (affine, linear))
            |= affine
            |= linear
        )
    ; succeed (fun linear -> (0, linear))
        |= linear
    ]

(* Formulae *)

let rec pattern' : unit -> pattern parser = fun () ->
  succeed (fun p -> p)
    |= one_of
         [ in_context "left pattern"
             ( succeed (fun phi -> MLeft phi)
                 |. keyword left_keyword
                 |. spaces1
                 |= lazily formula'
             )
         ; in_context "right pattern"
             ( succeed (fun phi -> MRight phi)
                 |. keyword right_keyword
                 |. spaces1
                 |= lazily formula'
             )
         ; in_context "product pattern"
             ( map (fun branches -> MTuple branches)
               ( sequence
                   ~start:prod_left_symbol
                   ~separator:prod_sep_symbol
                   ~endd:prod_right_symbol
                   ~spaces:spaces
                   ~item:
                     ( succeed (fun name r -> (name, r))
                         |= variable_name
                         |. spaces
                         |. symbol prod_constrains_symbol
                         |. spaces
                         |= lazily formula'
                     )
                   ~trailing:Forbidden
               )
             )
         ; in_context "nil pattern"
             ( ignore_with MNil (symbol nil_symbol)
             )
         ; in_context "cons pattern"
             ( succeed (fun phi phis -> MCons(phi, phis))
                 |= lazily formula'
                 |. spaces
                 |. symbol cons_symbol
                 |. spaces
                 |= lazily formula'
             )
         ]
    |. spaces

and atom' : unit -> formula parser = fun () ->
  succeed (fun a -> a)
    |= one_of
         [ in_context "false formula"
             ( ignore_with False (keyword false_keyword)
             )
         ; in_context "true formula"
             ( ignore_with True (keyword true_keyword)
             )
         ; in_context "leq formula"
             ( succeed (fun n1 n2 -> LEq (n1, n2))
                 |= number
                 |. spaces
                 |. symbol leq_symbol
                 |. spaces
                 |= number
             )
          ; in_context "match formula"
             ( succeed (fun p -> Match p)
                 |. keyword match_keyword
                 |. spaces
                 |. symbol lparen_symbol
                 |= lazily pattern'
                 |. symbol rparen_symbol
             )
         ]
    |. spaces

and disjunction_clause' : unit -> formula parser = fun () ->
  chainr1 "disjunction" (lazily atom')
    ( ignore_with (fun phi1 phi2 -> Or (phi1, phi2))
        ( succeed ()
            |. symbol or_symbol
            |. spaces
        )
    )

and formula' : unit -> formula parser = fun () ->
  lazily disjunction_clause'

let formula : formula parser =
  lazily formula'

(* Refinement types *)

let rec refine_atom' : unit -> refine parser = fun () ->
  succeed (fun a -> a)
    |= one_of
         [ in_context "void type"
             ( ignore_with RVoid (symbol void_symbol)
             )
         ; in_context "list type"
             ( succeed (fun inner -> RLst inner)
                 |. symbol list_left_symbol
                 |. spaces
                 |= lazily refine'
                 |. symbol list_right_symbol
             )
         ; in_context "refinement"
             ( succeed (fun r phi -> Refine (r, phi))
                 |. symbol refine_left_symbol
                 |. spaces
                 |= lazily refine'
                 |. symbol refine_mid_symbol
                 |. spaces
                 |= formula
                 |. symbol refine_right_symbol
             )
         ; in_context "product type"
             ( map (fun ts -> RProd ts)
               ( sequence
                   ~start:prod_left_symbol
                   ~separator:prod_sep_symbol
                   ~endd:prod_right_symbol
                   ~spaces:spaces
                   ~item:
                     ( succeed (fun name r -> (name, r))
                         |= variable_name
                         |. spaces
                         |. symbol prod_ascription_symbol
                         |. spaces
                         |= lazily refine'
                     )
                   ~trailing:Forbidden
               )
             )
         ]
    |. spaces

and refine_sum' : unit -> refine parser = fun () ->
  chainr1 "sum type" (lazily refine_atom')
    ( ignore_with (fun r1 r2 -> RSum (r1, r2))
        ( succeed ()
            |. symbol sum_symbol
            |. spaces
        )
    )

and refine' : unit -> refine parser = fun () ->
  lazily refine_sum'

let refine : refine parser =
  lazily refine'

let parse parser s =
  match Bark.run parser s with
    | Ok r -> Ok r
    | Error dead_ends ->
      let show_dead_end dead_end =
        match dead_end.Bark.problem with
          | Expecting s -> "'" ^ s ^ "'" in
      let msg = dead_ends
          |> List.map show_dead_end
          |> String.concat " | "
          |> (^) "Expecting " in
      Error msg
