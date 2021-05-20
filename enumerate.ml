open Utils
open Lang


(* Combinatorial helpers *)

let choose l r =
  let rec checked l r =
    if compare_length_with l r < 0 then
      Seq.empty else
    unchecked l r
  and unchecked l r () =
    if r = 0 then
      Seq.return Seq.empty () else
    match l () with
    | Seq.Nil -> assert false
    | Seq.Cons(h, l) -> Seq.append
      (Seq.map (Seq.cons h) (unchecked l (r - 1)))
      (checked l r) () in
  checked l r

let rec cross l r : 'p Seq.t = fun () ->
  Seq.flat_map (fun l -> Seq.map (fun r -> (l, r)) r) l ()

let rec product ll () = match ll () with
  | Seq.Cons(l, ll) -> Seq.flat_map (fun e -> Seq.map (Seq.cons e) (product ll)) l ()
  | Seq.Nil -> Seq.return Seq.empty ()

let repeat ~n a =
  Seq.unfold (function 0 -> None | n -> Some(a, n - 1)) n

let expand ?s ~n f =
  Seq.flat_map f (range ?s (n + 1))


(* Grammar enumeration *)

let cinit ~cmax =
  range ~s:1 cmax
  
let ninit ~tmax ~cmax t : number Seq.t =
  let rec linit ?(s = []) t = match t with
    | RSum(l, r) -> Seq.empty
    | RProd ts -> let ts = List.to_seq ts in
      Seq.flat_map (fun (n, t) -> linit ~s:(n :: s) t) ts
    | RLst t -> Seq.return (path s)
    | Refine(t, phi) -> linit ~s t
    | RVoid -> assert false in
  let terms = cross (linit t) (cinit ~cmax) in
  Seq.map (fun l -> (0, to_list l))
    (expand ~n:tmax (fun k -> choose terms k))

let finit ~fmax ~tmax ~cmax t =
  let ns = ninit ~tmax ~cmax t in
  let leqs = cross ns ns in
  Seq.map begin fun l -> match l () with
    | Seq.Cons((l, r), leqs) ->
      let or_ phi (l, r) = Or(phi, LEq(l, r)) in
      Seq.fold_left or_ (LEq(l, r)) leqs
    | Seq.Nil -> True
  end (expand ~n:fmax (fun k -> choose leqs k))

let rinit ~dmax ~pmax ~fmax ~tmax ~cmax =
  let rec rinit depth =
    (* FIX: disabled ORs in nested types *)
    let fmax = if depth = dmax then fmax else 1 in
    if depth = 0 then Seq.empty, Seq.return (RProd []) else
    let name = List.mapi (fun i t -> (string_of_int i, t)) in
    let prev, last = rinit (depth - 1) in
    (Seq.append prev last, concat [
      Seq.map (fun (l, r) -> RSum(l, r))
        (concat [cross prev last; cross last prev; cross last last])
    ; Seq.map (fun ts -> RProd(name (to_list ts)))
        (expand ~s:1 ~n:pmax (fun n -> Seq.flat_map product
          (skip 1 (product (repeat ~n (List.to_seq [prev; last]))))))
    ; Seq.map (fun t -> RLst t) last
    ; Seq.flat_map begin fun t ->
        Seq.map (fun phi -> Refine(t, phi))
          (finit ~fmax ~tmax ~cmax t)
      end last
    ]) in
  let prev, last = rinit dmax in
  Seq.append prev last

let rec einit ~lmax t = match t with
    | Sum(l, r) -> Seq.append
      (Seq.map (fun e -> L(e, r)) (einit ~lmax l))
      (Seq.map (fun e -> R(l, e)) (einit ~lmax r))
    | Prod [] -> Seq.return (Tuple [])
    | Prod((x, t) :: ts) ->
      Seq.flat_map begin fun e ->
        let es = let Tuple es = e in es in
        Seq.map (fun e -> Tuple((x, e) :: es)) (einit ~lmax t)
      end (einit ~lmax (Prod ts))
    | Lst t ->
      let es = einit ~lmax t in
      Seq.map (fun l -> Ls(t, to_list l))
        (expand ~n:lmax (fun n -> product (repeat ~n es)))
    | Void -> assert false
