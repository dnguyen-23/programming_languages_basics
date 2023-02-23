open Core
(* Today's goal will be to create a parser *)
(* What does a parser do: Converts your s-expression code to AST terms (see line 7) *)
(* Sometimes instead of writing "Num (123)", you just want to write "123" *)
(* This is when you need a parser to parse the "123" as "Num (123)" *)

(* Parser should take "expression" of type string; returns AST "term"*)
(* The "term" returned could be one large "term" composed of many other smaller "terms" *)

(* You can use s-expressions for something... *)
(* Why use s-expressions again: s-expr are for your own simplified language
   At the end of the day, you have to convert the s-expresions to AST terms *)





(* What are s-expressions? *)
(* ----s-expressions is the middle-man between your own language and
   ocaml's AST terms *)

   (* What do s-expressions look like*)
   (* Ocaml s-expressions include 
   "Atoms" or "Lists"
   
   "lists" are parentheses that include a series of "atoms"
   *)
   
   (* They look sort of like strings in parentheses 
   ex.) (1 2 3 4) 
   ex.) (func 1 2)
    *)


(* Converting from string to Sexp *)
let thingy = Sexp.of_string ("(1 2 3 4 5 6)");; (*Note: of_variable gen. means to convert FROM that type*)
(* Converting from Sexp to ocaml*)
let thingy2 = List.t_of_sexp Int.t_of_sexp thingy;;
(* let thingy2 = t_of_sexp thingy;; *)
Caml.print_endline "";;
List.map ~f:(fun x -> Caml.print_int x) thingy2;;
Caml.print_endline "";;

(* ppx_jane seems to have a parser t_of_sexp, but you should learn how to implement your own *)
(* let str_expr = "(1 2 3 4 5 6)";; *)
let str_expr = "(1 2 3 4 5)";;

(* let s_expr = Sexp.of_string (str_expr);;
let rec parse s_expr =
  match s_expr with
  | Sexp.List l -> 
    failwith "idk"
  | Sexp.Atom a ->
    (*Str.string_match is a function that takes 3 parameters
       1.) regex pattern
       2.) string to look at
       3.) start position
       *)
       (match a with
       | "true" | "false" -> Bool.to_int (Bool.of_string a)
       | _ -> 
        (* Using regex to check if string is int *)
        let is_int = (Str.string_match (Str.regexp "[0-9]+$") a 0) in
        if is_int then Int.of_string a
        else 
          let is_float = (Str.string_match (Str.regexp "^[0-9]+.[0-9]+$") a 0) in
          if is_float then Float.of_string a
          else a
        )
  | _ -> failwith "nothing to spit up"
  ;; *)
    
let rec parse (s_expr : Sexp.t) term = 
  match s_expr with
  | Sexp.List l -> 
    List.map ~f: (fun x -> parse x) l
  | Sexp.Atom a ->
  | _ -> failwith "idk"
;;

let s_expr2 = Sexp.of_string str_expr;;
parse s_expr2;;
