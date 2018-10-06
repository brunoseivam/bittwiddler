%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token TEMPLATE PARSE FUNCTION RETURN
%token FOR IN MATCH ARM IF ELSE
%token DOT COMMA COLON AT SEMICOLON ASSIGN
%token PLUS MINUS TIMES DIV REM
%token LSHIFT RSHIFT BWOR BWAND BWNOT
%token AND OR NOT
%token LT LTEQ EQ GT GTEQ
%token <string> ID
%token <string * int * string> INT_T
%token <int> FLOAT_T
%token STRING_T TYPE_T ARRAY_T FUNC_T TEMPLATE_T NONE_T
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token EOF

%start program
%type <Ast.program> program

%%

program:
    STRING EOF { StringLit($1) }
