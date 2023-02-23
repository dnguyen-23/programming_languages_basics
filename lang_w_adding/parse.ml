open Core
(* must open Core which contains s-expression resources *)



(* What does a parser do: Converts your code to AST terms (see line 7) *)
(* Sometimes instead of writing "Num (123)", you just want to write "123" *)
(* This is when you need a parser to parse the "123" as "Num (123)" *)

(* Parser should take "expression" of type string; returns AST "term"*)
(* The "term" returned could be one large "term" composed of many other smaller "terms" *)


(* Why use s-expressions again: s-expr are for your own simplified language
   At the end of the day, you have to convert the s-expresions to AST terms *)
   (* convert things from your own language to ocaml *)



   
(* What do s-expressions look like*)
(* Ocaml s-expressions include 
  "Atoms" or "Lists"

  "lists" are parentheses that include a series of "atoms"
  *)

(* They look sort of like strings in parentheses 
   ex.) (func 1 2)
    *)


(* We have to take in a string to start and convert it to an s-expression *)
let thingy = Sexp.of_string ("(1 2 3 4 5 6)");;
let thingy2 = List.t_of_sexp Int.t_of_sexp thingy;;

let rec parse (expr : string) term = 
  (* Converting the string to an s-expr *)
  Sexp.of_string (expr);;