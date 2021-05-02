open Lang
open Utils

let rec tselect t = function
  | Dot(p, x) -> let Prod ts = tselect t p in List.assoc x ts
  | Val -> t

let rec eselect e = function
  | Dot(p, x) -> let Tuple es = eselect e p in List.assoc x es
  | Val -> e

let rec slen = function
  | Proj((x, e)) -> slen (eselect e (Dot(Val, x)))
  | Append(e1, e2) -> add (slen e1) (slen e2)
  | Flatten(c, e) -> mult c (slen e)
  | Map((x, e1), e2) -> slen e2
  | V "val" -> len Val
  | _ -> assert false

let rec nrewrite i n =
  let ebody = i (V "val") in
  let e' p = eselect ebody p in
  let n' = function
    | p, c -> mult c (slen (e' p)) in
  List.fold_left add [] (List.map n' n)

let rec frewrite i = function
  | (False | True) as phi -> phi
  | Or(phi1, phi2) -> Or(frewrite i phi1, frewrite i phi2)
  | LEq(n1, n2) -> LEq(nrewrite i n1, nrewrite i n2)

let rearrange ((l, r): number * number) : number * number =
  let eqsplit = List.partition_map begin function
    | pi, ci when ci > 0 -> Either.Left (pi, ci)
    | pi, ci -> Either.Right (pi, -ci)
  end in
  eqsplit (add l (mult (-1) r))

let rec twith pt ?(p = Val) t =
  match List.assoc_opt p pt with
    | Some t -> t
    | None -> match t with
      | Sum _ | Lst _ -> t
      | Prod nts ->
        let twith (n, t) =
          let p = Dot(p, n) in
            (n, twith pt ~p t) in
        Prod(List.map twith nts)
      | Void -> assert false

let rec ebuild pe ?(p = Val) ~v t =
  match List.assoc_opt p pe with
    | Some e -> e
    | None -> match t with
      | RSum _ | RLst _ -> v
      | RProd nts ->
        let ebuild (n, t) =
          let p, v = Dot(p, n), Proj(n, v) in
            (n, ebuild pe ~p ~v t) in
        Tuple(List.map ebuild nts)
      | Refine(t, phi) -> ebuild pe ~p ~v t
      | RVoid -> assert false

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
  | Refine (rt, (LEq(n1, n2) as phi)) ->
    let i, t = simplify rt in
    begin match frewrite i phi with
      | LEq(n1, n2) ->
        let nu, nv = rearrange (n1, n2) in
        let (pu, cu), (qv, dv) =
          List.split nu, List.split nv in
        let lselect p =
          let Lst t' = tselect t p in t' in
        let tu =
          let power t i =
            Prod(List.init i (fun i -> (string_of_int i, t))) in
          List.map begin fun (pu, cu) ->
            Prod(List.mapi begin fun i (qv, dv) ->
                let luv = lcm cu dv in
                let au, bv = lselect pu, lselect qv in
                (string_of_int i, Lst(Prod(["a", power au (luv / cu);
                                            "b", power bv (luv / dv)])))
              end nv)
          end nu in
        let tv =
          List.map lselect qv in
        let concat (hd :: tl) =
          List.fold_left (fun l r -> Append(l, r)) hd tl in
        let epu v =
          List.map begin fun (pu, cu) ->
            concat (List.mapi begin fun i (qv, dv) ->
                let luv = lcm cu dv in
                Flatten(luv / dv, Map(("x", Proj("a", V "x")), Proj(string_of_int i, v)))
              end nv)
          end nu in
        let eqv v =
          List.mapi begin fun i (qv, dv) ->
            concat (List.map begin fun (pu, cu) ->
                let luv = lcm cu dv in
                Flatten(luv / dv, Map(("x", Proj("b", V "x")), Proj(string_of_int i, v)))
              end nu)
          end nv in
        let t' = twith (List.combine pu tu @ List.combine qv tv) t in
        let i' v = ebuild (List.combine pu (epu v) @ List.combine qv (eqv v)) ~v rt in
        (i' >> i, t')
      | _ -> assert false
      end
  | Refine(_, False) | RVoid ->
    let i v = assert false in
    (i, Void)
  | Refine(rt, True) ->
    simplify rt

(**** to string helpers ****)

let snames so (x, o) = x ^ ":" ^ so o

let rec ssimple = function
  | Sum(l, r) -> ssimple l ^ " + " ^ ssimple r
  | Prod ts -> "<" ^ String.concat ", " (List.map (snames ssimple) ts) ^ ">"
  | Lst t -> "[" ^ ssimple t ^ "]"
  | Void -> "_|_"

let rec spath = function
  | Dot(p, x) -> spath p ^ "." ^ x
  | Val -> "val"

let snumber (n: number) =
  let spc (p, c) = string_of_int c ^ match p with
    | p -> "len " ^ spath p in
  String.concat " + " (List.map spc n)

let rec sformula = function
  | False -> "F" | True -> "T"
  | Or(l, r) -> sformula l ^ " V " ^ sformula r
  | LEq(l, r) -> snumber l ^ " <= " ^ snumber r

let rec srefine = function
  | RSum(l, r) -> srefine l ^ " + " ^ srefine r
  | RProd ts -> "<" ^ String.concat ", " (List.map (snames srefine) ts) ^ ">"
  | Refine(t, phi) -> "{ " ^ srefine t ^ " | " ^ sformula phi ^ " }"
  | RLst t -> "[" ^ srefine t ^ "]"
  | RVoid -> "_|_"

(**** demo ****)

module ParserTests = struct
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
  ParserTests.run ()
