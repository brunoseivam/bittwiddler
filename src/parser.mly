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

parse:    PARSE block { Parse($2) }
template: TEMPLATE id params_opt block { Template($2, List.rev $3, $4) }
func:     FUNCTION id COLON typename params_opt block { Func($2, $4, List.rev $5, $6) }
param:    id COLON typename { Param($1, $3) }
id:       ID { Id($1) }

typename:
    id          { TCustom($1) }
  | INT_T       { TInt($1)    }
  | FLOAT_T     { TFloat($1)  }
  | STRING_T    { TNone       }

params:
    param              { [$1]     }
  | params COMMA param { $3 :: $1 }

params_opt:
    /* empty */          { [] }
  | LPAREN params RPAREN { $2 }

  block: LBRACE RBRACE { Block([]) }

pdecls_opt:
    /* empty */     { [] }
  | pdecls          { $1 }

pdecls:
    pdecl           { [$1] }
  | pdecls pdecl    { $2 :: $1 }

pdecl:
    template    { $1 }
  | func        { $1 }

program:
    pdecls_opt parse EOF { Program(List.rev $1,$2) }

