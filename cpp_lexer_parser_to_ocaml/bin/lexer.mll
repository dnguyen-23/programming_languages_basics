{
    open Parser
    open Lexing

    let next_line lexbuf = 
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- (* value being returned *)
        {
            pos with pos_bol = lexbuf.lex_curr_pos
                     pos_lnum = pos.pos_lnum + 1
        }
}

(* Defining the regexs for pattern matching the tokens *)

(* Requires numbers *)
let digit = ['0'-'9']
let int = '-'?digit+
let float = '-'?digit+'.'digit+

(* Requires letters *)
let letter = ['a'-'z' 'A'-'Z']

let id = letter^ (letter|digit|'_')+

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read_token = 
parse
(* Reading basic data types *)
| int {INT (int_of_string (Lexing.lexeme lexbuf))}
| float {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
| '"' {STIRNG (read_string (Buffer.create 17) lexbuf)}
| "'" {CHAR (read_char lexbuf)}
| "void" {VOID}
| "true" | "false" {BOOL (bool_of_string Lexing.lexeme lexbuf)}
(* Reading identifier -> variables, functions, names etc. *)
| id {ID (Lexing.lexeme lexbuf)}
| whitespace {read_token lexbuf} (* Keep reading for the next token*)
| newline {next_line lexbuf; read_token lexbuf} (* Switch onto the next line, 
                                                    continue reading for tokens *)

(* Reading syntax symbols *)
| "{" {LCURLY_BRACE}
| "}" {RCURLY_BRACE}
| "(" {LPAREN}
| ")" {RPAREN}
| "[" {LBRACK}
| "]" {RBRACK}
| ";" {SEMI_COLON}
| ":" {COLON}
| "," {COMMA}
(* Reading classes and control structures *)
| "class" {CLASS}
| "private" {PRIVATE}
| "public" {PUBLIC}
| "." {DOT}
| "if" {IF}
| "else" {ELSE}
| "for" {FOR}
| "while" {WHILE}
| "switch" {SWITCH}
| "case" {CASE}
(* Comments *)
| "//" {failwith "necessary?"}
(* Reading operators *)
| "+" {PLUS}
| "-" {MINUS}
| "*" {MULTIPLY}
| "/" {DIVIDE}
| "||" {OR}
| "&&" {AND}
| "!" {NOT}
| "==" {BOOL_EQUALS}
| "%" {MODULUS}
| "++" {ADD_ONE}
| "=" {EQUALS}
| "#" {POUND}
| "include" {INCLUDE}
| "<" {LESS_THAN}
| ">" {GREATER_THAN}
| _ {failwith "NYI: Comments and Strings"}


(* this function deals with escaped characters 
as well as reading the string*)
and rule read_string buf = 
parse
| '"' {STRING (Buffer.contents buf)} (* this represents the end quotation *)
