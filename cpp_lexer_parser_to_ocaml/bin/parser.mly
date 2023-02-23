%{
    
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <bool> BOOL
%token VOID

%token <string> ID

%token LCURLY_BRACE
%token RCURLY_BRACE
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token SEMI_COLON
%token COLON
%token COMMA

// Control structures
%token IF
%token ELSE
%token FOR
%token WHILE
%token SWITCH
%token CASE

// Class
%token CLASS
%token PUBLIC
%token PRIVATE
%token DOT

// Operations
%token PLUS
%token MINUS
%token MULTiPLY
%token DIVIDE

// Logical operators
%token OR
%token AND

// Comparison operators
%token LESS_THAN
%token GREATER_THAN
%token NOT
%token BOOL_EQUALS

// Other operators
%token MODULUS
%token ADD_ONE
%token EQUALS //assignment operator


//symbols
%token POUND
%token INCLUDE

%start program

%type <Ast.program> program

%program:
| libs = option(list(library_import)); fns = list(function_defn)

%library_import:
| POUND; INCLUDE; LESS_THAN; name=ID; GREATER_THAN; SEMI_COLON {TLibImp (name)}