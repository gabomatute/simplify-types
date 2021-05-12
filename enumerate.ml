open Lang

let expand ~n g f =
  let rec product ~n f =
    if n = 0 then f [] else
    product ~n:(n - 1) (fun es -> g (fun e -> f (e :: es))) in
  for n = 0 to n do product ~n f done

let rec tinit ~n ~d f =
  if d = 0 then f (Prod []) else
  let g = tinit ~n ~d:(d - 1) in
  g (fun t -> f (Lst t));
  g (fun l -> g (fun r -> f (Sum(l, r))));
  let name = List.mapi (fun i e -> (string_of_int i, e)) in
  expand ~n g (fun ts -> f (Prod(name ts)))

let rec einit ~n t f = match t with
    | Sum(l, r) ->
      einit ~n l (fun e -> f (L(e, r)));
      einit ~n r (fun e -> f (R(l, e)))
    | Prod [] -> f (Tuple [])
    | Prod((x, t) :: ts) ->
      einit ~n (Prod ts) begin fun e ->
        let es = let Tuple es = e in es in
        einit ~n t (fun e -> f (Tuple((x, e) :: es)))
      end
    | Lst t ->
      expand ~n (einit ~n t) (fun es -> f (Ls(t, es)))
    | Void -> assert false
