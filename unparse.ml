open Lang

let snamed ?(j = ", ") ?(v = ":") so os =
  let sname (n, o) =  n ^ v ^ so o in
  String.concat j (List.map sname os)

let rec ssimple = function
  | Sum(l, r) -> ssimple l ^ " + " ^ ssimple r
  | Prod ts -> "<" ^ snamed ssimple ts ^ ">"
  | Lst t -> "[" ^ ssimple t ^ "]"
  | Void -> "_|_"

let rec sexp ?(d = 1) st =
  let br i =
    let indent = String.make (i * 2) ' ' in "\n" ^ indent in
  function
  | V n -> (n : string)
  | L(e, t) -> "Left " ^ sexp ~d st e
  | R(t, e) -> "Right " ^ sexp ~d st e
  | Case(e, l, r) -> let c, d = d, d + 1 in
    let sb t (n, e) = br c ^ t ^ " " ^ n ^ " -> " ^ sexp ~d st e in
    "Case " ^ sexp ~d st e ^ " of " ^ sb "Left" l ^ ";" ^ sb "Right" r
  | Tuple [] -> "<>" (* don't indent *)
  | Tuple es -> let c, d = d, d + 1 in
    "<" ^ br d ^ snamed ~j:("," ^ br d) ~v:" = " (sexp ~d st) es ^ br c ^ ">"
  | Proj(n, e) -> "proj_{" ^ n ^ "} " ^ sexp ~d st e
  | Ls(t, es) -> "[" ^ (String.concat "; " (List.map (sexp ~d st) es)) ^ "]"
  | Nil t -> "[]" | Cons(e, es) -> sexp ~d st e ^ "::" ^ sexp ~d st es
  | Map((n, f), e) -> "map (λ" ^ n ^ ". " ^ sexp ~d st f ^ ") " ^ sexp ~d st e
  | Append(l, r) -> sexp ~d st l ^ " ++ " ^ sexp ~d st r
  | Flatten(i, e) -> "flatten_" ^ string_of_int i ^ " " ^ sexp ~d st e

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
  | Match p -> "match(" ^ spattern p ^ ")"

and spattern = function
  | MLeft phi -> "Left " ^ sformula phi
  | MRight phi -> "Right " ^ sformula phi
  | MTuple phis -> "<" ^ snamed ~v:"~" sformula phis ^ ">"

let rec srefine = function
  | RSum(l, r) -> srefine l ^ " + " ^ srefine r
  | RProd ts -> "<" ^ snamed srefine ts ^ ">"
  | Refine(t, phi) -> "{ " ^ srefine t ^ " | " ^ sformula phi ^ " }"
  | RLst t -> "[" ^ srefine t ^ "]"
  | RVoid -> "_|_"
