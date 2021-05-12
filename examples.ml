open Utils
open Lang
open Simplify
open Parse
open Unparse

let rparse s =
  match parse refine s with
  | Error msg -> raise (Invalid_argument ("Parse Error: " ^ msg))
  | Ok rt -> rt

let showcase desc s =
  let rt = rparse s in
  print_endline ("```");
  print_endline ("# " ^ desc);
  print_endline ("input  = " ^ srefine rt);
  let i, t = simplify rt in
  print_endline ("output = " ^ ssimple t);
  print_endline ("```");
  ;;

let () =
  print_endline "# Potential Applications";
  print_endline "Below are some examples of potential applications of this technique.";
  print_endline "";
  print_endline "**Note:** `output` is automatically generated from the latest version of the tool.";
  print_endline "";
  print_endline "## Guarantee Correctness";
  print_endline "Make illegal states unrepresentable by specifying constraints to generate a new type.";
  showcase
    "e.g. Given a list of questions and a list of answers, ensure they have the same length."
    "{{<q:[<>], a:[<>]> | len val.q <= len val.a} | len val.a <= len val.q}";
  print_endline "";
  print_endline ("## Guarantee Performance");
  print_endline "Ensure certain algorithmic assumptions hold when implementing efficient data structures.";
  showcase
  "e.g. Okasaki functional requires front to be shorter than the back for amortized O(1) operations."
  "{<front: [<>], back: [<>]> | len val.front <= len val.back }";
  print_endline "";
  print_endline "## Improve Performance";
  print_endline "Potentially could use known properties to automatically prune unused states from your types.";
  showcase
    "e.g. Automatically drop type union tags and empty list pointers when uneeded."
    "{ [<>] + [<>] | match (Left len val <= 0) }";
  print_endline "";
  print_endline "## Improve Interfaces";
  print_endline "Design library interfaces that can be less error prone by encoding constraints into the type system.";
  showcase
    "e.g. Encode that a random choice function requires a non-empty list of choices."
    "{<choices:[<>]> | match(<choices ~ match(T :: T)>)}";
