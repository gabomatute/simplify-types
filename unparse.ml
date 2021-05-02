open Lang

let snamed so os =
  let sname (n, o) =  n ^ ":" ^ so o in
  String.concat ", " (List.map sname os)

let rec ssimple = function
  | Sum(l, r) -> ssimple l ^ " + " ^ ssimple r
  | Prod ts -> "<" ^ snamed ssimple ts ^ ">"
  | Lst t -> "[" ^ ssimple t ^ "]"
  | Void -> "_|_"

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