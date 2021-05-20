let (>>) f g x =
  g (f x)

let (let* ) = Option.bind

module List = struct
  include List
  let rec assoc_update f k ?v = function
    | (ki, vi) :: rest when ki = k -> (ki, f vi) :: rest
    | hd :: rest -> hd :: assoc_update f k ?v rest
    | [] -> [(k, f (Option.get v))]
end

module Seq = struct
  include Seq

  let rec compare_length_with l n = match l (), n with
    | Seq.Cons _, 0 -> 1 | Seq.Nil, 0 -> 0 | Seq.Nil, n -> -n
    | Seq.Cons(h, l), n -> compare_length_with l (n - 1)

  let rec concat = function
    | hd :: tl -> Seq.append hd (fun () -> concat tl ())
    | [] -> Seq.empty

  let rec skip n l =
    if n = 0 then l else
    match l () with
      | Seq.Cons(_, l) -> skip (n - 1) l
      | Seq.Nil -> assert false

  let to_list l =
    List.rev (Seq.fold_left (fun l e -> e :: l) [] l)

  let rec range ?(s = 0) n () =
    if s < n then Seq.Cons(s, range ~s:(s + 1) n)
    else Seq.Nil
end

let uppercase_char : char -> bool =
  function
    | 'A' .. 'Z' -> true
    | _ -> false

let lowercase_char : char -> bool =
  function
    | 'a' .. 'z' -> true
    | _ -> false

let digit_char : char -> bool =
  function
    | '0' .. '9' -> true
    | _ -> false

let rec gcd u = function
  | 0 -> abs u | v -> gcd v (u mod v)
  
let lcm m n = match m, n with
  | 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / (gcd m n)
