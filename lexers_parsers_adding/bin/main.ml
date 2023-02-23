(* Comes from "ast.ml" file*)
open Ast

let pretty_print str_thing = 
  Caml.print_endline "";
  Caml.print_endline str_thing
;;

(* Parses the string into an AST *)
let parse (str_to_parse : string) : expr = 
  (* 1.) Tokenize the string *)
  let lexbuf = Lexing.from_string str_to_parse in
  Parser.program Lexer.read_token lexbuf
;;

let rec print_result (result : expr) = 
  match result with
  | Num n -> pretty_print (Int.to_string n)
  | Plus (e1, e2) -> 
    print_result e1;
    Caml.print_char ' ';
    Caml.print_char '+';
    Caml.print_char ' ';
    print_result e2
  (* | _ -> failwith "not an int" *)
;;

let value = parse "(1 + 2)" in
print_result value;;