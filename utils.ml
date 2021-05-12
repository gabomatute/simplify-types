let (>>) f g x =
  g (f x)

let (let* ) = Option.bind

let rec assoc_update f k ?v = function
  | (ki, vi) :: rest when ki = k -> (ki, f vi) :: rest
  | hd :: rest -> hd :: assoc_update f k ?v rest
  | [] -> [(k, f (Option.get v))]

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
