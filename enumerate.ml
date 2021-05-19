open Utils
open Lang

let rec product ll () = match ll () with
  | Seq.Cons(l, ll) -> Seq.flat_map (fun e -> Seq.map (Seq.cons e) (product ll)) l ()
  | Seq.Nil -> Seq.return Seq.empty ()

let repeat ~n a =
  Seq.unfold (function 0 -> None | n -> Some(a, n - 1)) n

let expand ?s ~n f =
  Seq.flat_map f (range ?s (n + 1))

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
