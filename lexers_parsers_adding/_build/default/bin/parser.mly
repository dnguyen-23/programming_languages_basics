%{
    [@@@ coverage exclude_file]
    (*open Ast.Ast_types
    open Parsed_ast *)
%}

// Behind the scenes, there is a variant type "token" created for parsing
// These "%token" expressions tell the Ocaml parser to create a new constructor in the "token" variant type
%token <int> Num
%token Lparen
%token Rparen
%token Plus
%token Eof //the lexer will return this when it reaches the end of the string being parsed


// %start tells the parser where to start looking for parsing rules
%start <Ast.expr> program //the <Ast.expr> says returning the "expr" node 
                          //from the "Ast" module you made from "ast.ml"

%% //This thing separates the declarations from the rules that you will define below


// Rule taking the entire program from the file being parsed (nothing left in file)
// Program will be taken for parsing
program:
    | e = expr; Eof { e } //"expr" in this case isn't a grammar production; it isn't an AST type; it's a nonterminal
    ;

// Rule for parsing expressions
expr:
    | Lparen; e = expr; Rparen { e }
    | e1 = expr; Plus; e2 = expr {Plus(e1, e2)}
    | i = Num { Num i}
    ;