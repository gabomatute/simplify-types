open Utils
open Lang
open Simplify
open Parse
open Unparse

let rparse s =
  match parse refine s with
  | Error msg -> raise (Invalid_argument ("Parse Error: " ^ msg))
  | Ok rt -> rt

let showcase s =
  let rt = rparse s in
  print_endline ("```");
  print_endline ("input  = " ^ srefine rt);
  let i, t = simplify rt in
  print_endline ("output = " ^ ssimple t);
  print_endline ("```");
  ;;

let () =
  print_endline ("# Potential Applications");
  print_endline ("");
  print_newline ();
  print_endline ("## Guarantee Correctness");
  print_endline ("Make illegal states unrepresentable by adding constraints and generating a new type.");
  showcase "{{<q:[<>], a:[<>]> | len val.q <= len val.a} | len val.a <= len val.q}";
  print_newline ();
  print_endline ("## Efficiency");
  print_endline ("Use known properties to automatically prune unused states from your types.");
  showcase "{ [<>] + [<>] | match (Left len val <= 0len val) }";
  print_newline ();
  print_endline ("## Remove Partiality");
  print_endline ("Make libraries more user-friendly, less error prone by encoding constraints into any type system.");
  showcase "{<choices:[<>]> | match(<choices ~ match(T :: T)>)}";
