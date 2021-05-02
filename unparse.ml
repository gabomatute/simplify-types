open Lang

let snamed ?(v = ":") so os =
  let sname (n, o) =  n ^ v ^ so o in
  String.concat ", " (List.map sname os)

let rec ssimple = function
  | Sum(l, r) -> ssimple l ^ " + " ^ ssimple r
  | Prod ts -> "<" ^ snamed ssimple ts ^ ">"
  | Lst t -> "[" ^ ssimple t ^ "]"
  | Void -> "_|_"

let rec sexp = function
  | V n -> n
  | L e -> "Left " ^ sexp e
  | R e -> "Right " ^ sexp e
  | Case(e, l, r) ->
    let sb c (n, e) = c ^ " " ^ n ^ " -> " ^ sexp e in
    "Case " ^ sexp e ^ " of " ^ sb "Left" l ^ "; " ^ sb "Right" r
  | Tuple es -> "<" ^ snamed ~v:" = " sexp es ^ ">"
  | Proj(n, e) -> "proj_{" ^ n ^ "} " ^ sexp e
  | Append(l, r) -> sexp l ^ " ++ " ^ sexp r
  | Flatten(i, e) -> "flatten_" ^ string_of_int i ^ " " ^ sexp e
  | Map((n, f), e) -> "map (λ" ^ n ^ ". " ^ sexp f ^ ") " ^ sexp e

let rec spath = function
  | Dot(p, x) -> spath p ^ "." ^ x
  | Val -> "val"

let snumber (n: number) =
  String.concat " + " (List.map begin function
    | p, c -> string_of_int c ^ "len " ^ spath p
  end n)

let rec sformula = function
  | False -> "F" | True -> "T"
  | Or(l, r) -> sformula l ^ " V " ^ sformula r
  | LEq(l, r) -> snumber l ^ " <= " ^ snumber r

let rec srefine = function
  | RSum(l, r) -> srefine l ^ " + " ^ srefine r
  | RProd ts -> "<" ^ snamed srefine ts ^ ">"
  | Refine(t, phi) -> "{ " ^ srefine t ^ " | " ^ sformula phi ^ " }"
  | RLst t -> "[" ^ srefine t ^ "]"
  | RVoid -> "_|_"