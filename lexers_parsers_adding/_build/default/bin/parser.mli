
(* The type of tokens. *)

type token = 
  | Rparen
  | Plus
  | Num of (int)
  | Lparen
  | Eof

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
