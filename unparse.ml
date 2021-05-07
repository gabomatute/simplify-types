open Lang

let snamed ?(v = ":") so os =
  let sname (n, o) =  n ^ v ^ so o in
  String.concat ", " (List.map sname os)

let rec ssimple = function
  | Sum(l, r) -> ssimple l ^ " + " ^ ssimple r
  | Prod ts -> "<" ^ snamed ssimple ts ^ ">"
  | Lst t -> "[" ^ ssimple t ^ "]"
  | Void -> "_|_"

let rec sexp st = function
  | V n -> (n : string)
  | L(e, t) -> "Left_{" ^ st t ^ "} " ^ sexp st e
  | R(t, e) -> "Right_{" ^ st t ^ "} " ^ sexp st e
  | Case(e, l, r) ->
    let sb c (n, e) = c ^ " " ^ n ^ " -> " ^ sexp st e in
    "Case " ^ sexp st e ^ " of " ^ sb "Left" l ^ "; " ^ sb "Right" r
  | Tuple es -> "<" ^ snamed ~v:" = " (sexp st) es ^ ">"
  | Proj(n, e) -> "proj_{" ^ n ^ "} " ^ sexp st e
  | Ls(t, es) -> "[" ^ (String.concat "; " (List.map (sexp st) es)) ^ "]"
  | Map((n, f), e) -> "map (λ" ^ n ^ ". " ^ sexp st f ^ ") " ^ sexp st e
  | Append(l, r) -> sexp st l ^ " ++ " ^ sexp st r
  | Flatten(i, e) -> "flatten_" ^ string_of_int i ^ " " ^ sexp st e

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