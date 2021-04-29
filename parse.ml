open Bark
open Bark.Syntax
open Lang
open Utils

(* Parser specialization *)

type problem =
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingLeftBracket
  | ExpectingRightBracket
  | ExpectingComma
  | ExpectingRightArrow
  | ExpectingLAngle
  | ExpectingRAngle
  | ExpectingSpace
  | ExpectingPound
  | ExpectingDot
  | ExpectingEquals
  | ExpectingDoubleEquals
  | ExpectingHole
  | ExpectingLambda
  | ExpectingPipe
  | ExpectingColon
  | ExpectingFuncSpec

  | ExpectingWildcard
  | ExpectingLineComment
  | ExpectingMultiCommentStart
  | ExpectingMultiCommentEnd

  | ExpectingExactly of int * int

  | ExpectingMoreIndent

  | ExpectingLet
  | ExpectingIn
  | ExpectingCase
  | ExpectingOf
  | ExpectingType
  | ExpectingAssert

  | ExpectingNat

  | ExpectingConstructorName
  | ExpectingVariableName
  | ExpectingHoleName

  | ExpectingFunctionArity

  | ExpectingTupleSize
  | ExpectingTupleIndex

  | ExpectingName of string * string

  | NegativeArity of int
  | ZeroArity

  | ExpectingEnd

type context =
  | CType
  | CTTuple
  | CTData
  | CTArr
  | CTForall
  | CTVar

  | CTypeParam
  | CTypeArg

  | CPat
  | CPTuple
  | CPVar
  | CPWildcard

type 'a parser =
  (context, problem, 'a) Bark.parser

(* Symbols *)

let left_paren =
  Token ("(", ExpectingLeftParen)

let right_paren =
  Token (")", ExpectingRightParen)

let left_bracket =
  Token ("[", ExpectingLeftBracket)

let right_bracket =
  Token ("]", ExpectingRightBracket)

let comma =
  Token (",", ExpectingComma)

let right_arrow =
  Token ("->", ExpectingRightArrow)

let langle =
  Token ("<", ExpectingLAngle)

let rangle =
  Token (">", ExpectingRAngle)

let pound =
  Token ("#", ExpectingPound)

let dot =
  Token (".", ExpectingDot)

let equals =
  Token ("=", ExpectingEquals)

let double_equals =
  Token ("==", ExpectingDoubleEquals)

let hole =
  Token ("??", ExpectingHole)

let lambda =
  Token ("\\", ExpectingLambda)

let pipe =
  Token ("|", ExpectingPipe)

let colon =
  Token (":", ExpectingColon)

let wildcard =
  Token ("_", ExpectingWildcard)

let line_comment_start =
  Token ("--", ExpectingLineComment)

let multi_comment_start =
  Token ("{-", ExpectingMultiCommentStart)

let multi_comment_end =
  Token ("-}", ExpectingMultiCommentEnd)

(* Keywords *)

let forall_keyword =
  Token ("forall", ExpectingLet)

let let_keyword =
  Token ("let", ExpectingLet)

let in_keyword =
  Token ("in", ExpectingIn)

let case_keyword =
  Token ("case", ExpectingCase)

let of_keyword =
  Token ("of", ExpectingOf)

let type_keyword =
  Token ("type", ExpectingType)

let assert_keyword =
  Token ("assert", ExpectingAssert)

(* No-lookahead keywords *)

let specify_function_token =
  Token ("specifyFunction", ExpectingFuncSpec)

(* Parser helpers *)

let optional : 'a parser -> 'a option parser =
  fun p ->
    one_of
      [ map (fun x -> Some x) p
      ; succeed None
      ]

type indent_strictness =
  | Strict
  | Lax

let check_indent : indent_strictness -> unit parser =
  fun indent_strictness ->
    let check_ok col indent =
      match indent_strictness with
        | Strict ->
            col > indent

        | Lax ->
            col >= indent
    in
    let* ok =
      succeed check_ok
        |= get_col
        |= get_indent
    in
    if ok then
      succeed ()
    else
      problem ExpectingMoreIndent

let with_current_indent : 'a parser -> 'a parser =
  fun p ->
    let* col =
      get_col
    in
    with_indent col p

(* Spaces *)

let if_progress : 'a parser -> int -> (int, unit) step parser =
  fun p offset ->
    let+ new_offset =
      succeed (fun n -> n)
        |. p
        |= get_offset
    in
    if Int.equal offset new_offset then
      Done ()
    else
      Loop new_offset

let any_spaces : unit parser =
  loop 0
    ( if_progress
        ( one_of
            [ line_comment line_comment_start
            ; multi_comment multi_comment_start multi_comment_end Nestable
            ; spaces
            ]
        )
    )

let single_line_spaces : unit parser =
  loop 0
    ( if_progress
        ( one_of
            [ line_comment line_comment_start
            ; chomp_while (Char.equal ' ')
            ]
        )
    )

(* Indented spaces *)

let sspaces : unit parser =
  succeed ()
    |. any_spaces
    |. check_indent Strict

let lspaces : unit parser =
  succeed ()
    |. any_spaces
    |. check_indent Lax

let tuple : ('a -> 'b) -> ('a list -> 'b) -> 'a parser -> 'b parser =
  fun single multiple item ->
    map
      ( fun inners ->
          match inners with
            | [inner] ->
                single inner

            | _ ->
                multiple inners
      )
      ( sequence
          ~start:left_paren
          ~separator:comma
          ~endd:right_paren
          ~spaces:lspaces
          ~item:item
          ~trailing:Forbidden
      )

let listt : 'a parser -> 'a list parser =
  fun item ->
    sequence
      ~start:left_bracket
      ~separator:comma
      ~endd:right_bracket
      ~spaces:lspaces
      ~item:item
      ~trailing:Forbidden

let wrapped_poly : 'a parser -> 'a list parser =
  fun item ->
    sequence
      ~start:langle
      ~separator:comma
      ~endd:rangle
      ~spaces:lspaces
      ~item:item
      ~trailing:Forbidden

let single_wrapped_poly : 'a parser -> 'a parser =
  fun item ->
    succeed (fun x -> x)
      |. symbol langle
      |. sspaces
      |= item
      |. sspaces
      |. symbol rangle

let exactly : int -> 'a parser -> 'a list parser =
  fun n p ->
    loop (n, [])
      ( fun (k, rev_xs) ->
          if k <= 0 then
            succeed (Done (List.rev rev_xs))
          else
            one_of
              [ map (fun x -> Loop (k - 1, x :: rev_xs)) p
              ; problem (ExpectingExactly (n, n - k))
              ]
      )

let chainl1 :
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

let chainr1 : context -> 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser =
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

