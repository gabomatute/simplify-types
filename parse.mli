open Lang

(** The possible parse errors. *)
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

(** The possible parse contexts. *)
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

(** The type of a parser. *)
type 'a parser =
  (context, problem, 'a) Bark.parser

(** Expression parser. *)
(* val exp : exp parser *)

(** Type parser. *)
val refine : refine parser
