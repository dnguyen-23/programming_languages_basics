{
open Parser
open Lexing

exception SyntaxError of string

exception SyntaxError of string

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                    pos_lnum = pos.pos_lnum + 1
        }


(* When the lexer is looking at the written code that it will translate,
    these will be the regex patterns that the lexer will use to translate 
    the code into your desired tokens
    ex. "let int..." holds the regex pattern used to identify a variable of type int 
    so that the lexer can assign it a TYPE_INT token *)
let digit = ['0'-'9']
let letter = [('a'-'z')|('A'-'Z')]

let int = ('-'?) (digit+$)
let float = ('-'?) (digit+) ('.') (digit+$)
let variable = (letter) (letter|digit|"_")*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* The next step is to define how the lexer will read your code
    There are 4 things that can be read:
    1.) a token
    2.) a string
    3.) a single line comment
    4.) a multi-line comment *)

(* how to structure this part of the code 

rule <rule_name> = parse
| <regex>  {  TOKEN_NAME } (* output a token *)
| <regex>  { ... } (*or execute other code*) 

*)
rule read_token =
parse
| int {INT (int_of_string (Lexing.lexeme lexbuf))}
| float {FLOAT (float_of_string (Lexing.lexeme lexbuf))} 
(*What is Lexing.lexeme lexbuf?*)
(* Lexing.lexeme lexbuf is a buffer that gets the string matched with the regexp *)
| variable {VAR (Lexing.lexeme lexbuf)}


}