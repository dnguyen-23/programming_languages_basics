{ (*These set of curly braces is where you can put ocaml code*)

(*By running dune exec or dune build, the lexer.ml file will already be made for you*)
open Lexing
open Parser
(* open Menhir.Parser *)

exception SyntaxError of string

(* This is for skipping over the newline character as the lexer goes through the program
    and matches parts of the program with the regex *)
let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p (*What's the point of this? *)
    (* lex_curr_p is a method returning a record of the current position of the lexer
            in the form of 3 variables: 
            1.) pos_fname -> the name of the file the lexer is reading from; the input file
            2.) pos_lnum -> the line number the lexer will be reading from
            3.) pos_bol -> the column number that the lexer will be reading from *)
    in
    lexbuf.lex_curr_p <-
    {
        pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
                 (*lexbuf.lex_curr_pos is different from lex_curr_p
                        lex_curr_POS returns just the column position (i.o.w just an updated pos_bol)*)
    }
}





(* Defining the regex patterns for recognizing the tokens *)
(* sample code: (1 2 3 4) *)
let digit = ['0'-'9']
let num = '-'?digit+
(* let vector = '('(('-'?digit+$)\s)+')'$ *) (*this is not a good idea and is risky*: what if there are sub lists*)
let plus = "+"

(* id will hold the regex for identifiers -> these are the names of variables and functions
(* This format starts with (alpha) to mandate that the identifier name must always start with
        a letter and not a number or any other character *)
let id = (alpha)(alpha|digit|"_") *)

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Lexing.lexeme lexbuf gives you the string that matched the regexp *)
rule read_token = parse
| num {Num(int_of_string (Lexing.lexeme lexbuf))}
| whitespace {read_token lexbuf}
| "(" {Lparen} (* this is important for parsing later*)
                (* Why: tokenizing a vector all at once wil be very difficult
                    This is because you could have nested vectors *)
| newline {next_line lexbuf; read_token lexbuf}
| plus {Plus}
| ")" {Rparen}
| eof {Eof}
| _ {raise (SyntaxError ("There was something wrong with this code:" ^ Lexing.lexeme lexbuf))}


(* simple list: (1 2 3 4 5)
    complex list: ((1 2) (2 3 4) (3 4 5 6))
 *)