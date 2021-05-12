open Utils
open Lang

let rec proj p v = match p with
  | Dot(p, x) -> Proj(x, proj p v)
  | Val -> v

let rec tselect t = function
  | Dot(p, x) -> let Prod ts = tselect t p in List.assoc x ts
  | Val -> t

let rec stack ?(s = []) = function
  | Dot(p, x) -> stack ~s:(x :: s) p
  | Val -> s

let rec path ?(p = Val) = function
  | x :: rest -> path ~p:(Dot(p, x)) rest
  | [] -> p

let rec slen ?(s = []) = function
  | Proj(n, e) -> slen ~s:(n :: s) e
  | Tuple es -> let n :: s = s in slen ~s (List.assoc n es)
  | Append(e1, e2) -> add (slen e1) (slen e2)
  | Flatten(c, e) -> mult c (slen e)
  | Map((x, e1), e2) -> slen e2
  | V "val" -> len (path s)
  | _ -> assert false

let rec nrewrite i n =
  let e = i (V "val") in
  let n' = function
    | p, c -> mult c (slen ~s:(stack p) e) in
  List.fold_left add [] (List.map n' n)

let rec frewrite i = function
  | (False | True) as phi -> phi
  | Or(phi1, phi2) -> Or(frewrite i phi1, frewrite i phi2)
  | LEq(n1, n2) -> LEq(nrewrite i n1, nrewrite i n2)
  | Match p -> Match(prewrite i p)

and prewrite i = function
  | MLeft phi -> MLeft(frewrite i phi)
  | MRight phi -> MRight(frewrite i phi)
  | MTuple phis ->
      MTuple(List.map (fun (n, phi) -> (n, frewrite i phi)) phis)

let rearrange ((l, r): number * number) : number * number =
  let rec eqsplit = function
    | [] -> [], []
    | (p, c) :: n -> let l, r = eqsplit n in
      if c < 0 then l, (p, -c) :: r else
      if c > 0 then (p, c) :: l, r else
      l, r in
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
    let i v = Case(v,
      ("x1", L(i1(V "x1"), bare rt1)),
      ("x2", R(bare rt2, i2(V "x2")))) in
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
  | Refine(RSum(rt1, rt2), Match(MLeft phi)) ->
    let i, t = simplify (Refine(rt1, phi)) in
    let i' v = L(v, bare rt2) in
    (i >> i', t)
  | Refine(RSum(rt1, rt2), Match(MRight phi)) ->
    let i, t = simplify (Refine(rt2, phi)) in
    let i' v = R(bare rt1, v) in
    (i >> i', t)
  | Refine(RProd ts, Match(MTuple phis)) ->
    let rt = RProd(List.map begin fun (n, t) ->
      match List.assoc_opt n phis with
        | Some phi -> (n, Refine(t, phi))
        | None -> (n, t)
      end ts) in
    simplify rt
  | Refine(t, Match p) ->
    assert false
  | Refine (rt, (LEq(n1, n2) as phi)) ->
    let i, t = simplify rt in
    begin match frewrite i phi with
      | LEq(n1, n2) ->
        let nu, nv = rearrange (n1, n2) in
        let (pu, cu), (qv, dv) =
          List.split nu, List.split nv in
        let tu =
          let power t i =
            if i = 1 then t else power t i in
          List.map begin fun (pu, cu) ->
            Prod(List.mapi begin fun i (qv, dv) ->
                let luv = lcm cu dv in
                let au = let Lst au = tselect t pu in au in
                let bv = let Lst bv = tselect t qv in bv in
                (string_of_int i, Lst(Prod(["α", power au (luv / cu);
                                            "β", power bv (luv / dv)])))
              end nv)
          end nu in
        let tv =
          List.map (tselect t) qv in
        let concat t = function
          | hd :: tl -> List.fold_left (fun l r -> Append(l, r)) hd tl
          | [] -> Ls(t, []) in
        let epu v =
          List.map2 begin fun tu (pu, cu) ->
            concat tu (List.mapi begin fun i (qv, dv) ->
                let luv'cu = lcm cu dv / cu in
                let extract = Proj(string_of_int i, proj pu v) in
                let rebuilt = Map(("x", Proj("α", V "x")), extract) in
                if luv'cu = 1 then rebuilt else
                Flatten(luv'cu, rebuilt)
              end nv)
          end tu nu in
        let eqv v =
          List.mapi begin fun i (qv, dv) ->
            let tv = List.nth tv i in
            concat tv ((List.map begin fun (pu, cu) ->
                let luv'dv = lcm cu dv / dv in
                let extract = Proj(string_of_int i, proj pu v) in
                let rebuilt = Map(("x", Proj("β", V "x")), extract) in
                if luv'dv = 1 then rebuilt else
                Flatten(luv'dv, rebuilt)
              end nu) @ [proj qv v])
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

let rec one = function
  | V _ | L _ | R _ | Case _ -> false
  | Tuple es -> List.for_all (fun (n, e) -> one e) es | Proj _ -> false
  | Ls _ | Nil _ | Cons _ | Map _ | Append _ | Flatten _ -> false

let rec optimize ~v = function
  | V n -> V n
  | L(e, t) -> L(optimize ~v e, t)
  | R(t, e) -> R(t, optimize ~v e)
  | Case(e, (ln, le), (rn, re)) ->
    let e = optimize ~v e in
    let lt, rt = let Sum(l, r) = ssyn ~vars:[v] e in l, r in
    begin match optimize ~v:(ln, lt) le, optimize ~v:(rn, rt) re with
    | L(V ln', _), R(_, V rn') when (ln', rn') = (ln, rn) -> e
    | le, re -> Case(e, (ln, le), (rn, re))
    end
  | Tuple es ->
    begin match Tuple(List.map (fun (n, e) -> (n, optimize ~v e)) es) with
    | Tuple((n, Proj(n', e)) :: es) when n' = n && List.for_all (function
      | n, Proj(n', e') when n' = n && e' = e -> true | _ -> false) es -> e
    | e when one e && ssyn e = snd v -> V(fst v)
    | e -> e
    end
  | Proj(n, e) ->
    begin match optimize ~v e with
    | Tuple es -> List.assoc n es
    | e -> Proj(n, e)
    end
  | Ls(t, es) -> Ls(t, List.map (optimize ~v) es)
  | Nil t -> Nil t
  | Cons(e, es) ->
    Cons(optimize ~v e, optimize ~v es)
  | Map((n, f), e) ->
    let e = optimize ~v e in
    let t = let Lst t = ssyn ~vars:[v] e in t in
    begin match optimize ~v:(n, t) f with
    | V n' when n' = n -> e
    | f -> Map((n, f), e)
    end
  | Append(l, r) ->
    begin match optimize ~v l, optimize ~v r with
    | e, Ls(_, []) | Ls(_, []), e -> e
    | l, r -> Append(l, r)
    end
  | Flatten(i, e) -> Flatten(i, optimize ~v e)