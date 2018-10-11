%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token TEMPLATE PARSE FUNCTION RETURN VAR
%token FOR IN MATCH ARM IF ELSE
%token DOT COMMA COLON AT DOLLAR SEMICOLON ASSIGN
%token PLUS MINUS TIMES DIV REM
%token LSHIFT RSHIFT BWOR BWAND BWNOT
%token AND OR NOT
%token LT LTEQ EQ NEQ GT GTEQ
%token <string> ID
%token <string * int * string> INT_T
%token <int> FLOAT_T
%token STRING_T TYPE_T ARRAY_T FUNC_T TEMPLATE_T NONE_T
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token EOF

%left DOT LBRACK RBRACK
%right NOT BWNOT
%left TIMES DIV REM
%left PLUS MINUS
%left LSHIFT RSHIFT
%left LT LTEQ GT GTEQ
%left EQ NEQ
%left BWAND
%left BWOR
%left AND
%left OR
%right ASSIGN
%right COMMA
%right AT DOLLAR

%start program
%type <Ast.program> program

%%

id:
    ID { Id($1) }

typename:
    INT_T       { TInt($1)    }
  | FLOAT_T     { TFloat($1)  }
  | STRING_T    { TString     }
  | id          { TCustom($1) }

param:
    id COLON typename { Param($1, $3) }

params:
    param              { [$1]     }
  | params COMMA param { $3 :: $1 }

params_opt:
    /* empty */          { [] }
  | LPAREN params RPAREN { $2 }

template:
    TEMPLATE id params_opt block { Template($2, List.rev $3, $4) }

func:
    FUNCTION id COLON typename params_opt block { Func($2, $4, List.rev $5, $6) }

opt_hide:
    /* empty */ { false }
  | AT          { true  }

var:
    VAR opt_hide id   COLON expr ASSIGN expr SEMICOLON {  Var($2, $3, Some $5, Some $7) }
  | VAR opt_hide id   COLON expr             SEMICOLON {  Var($2, $3, Some $5, None   ) }
  | VAR opt_hide id              ASSIGN expr SEMICOLON {  Var($2, $3, None,    Some $5) }
  | VAR DOLLAR   expr COLON expr ASSIGN expr SEMICOLON { TVar(    $3, Some $5, Some $7) }
  | VAR DOLLAR   expr COLON expr             SEMICOLON { TVar(    $3, Some $5, None   ) }
  | VAR DOLLAR   expr            ASSIGN expr SEMICOLON { TVar(    $3, None,    Some $5) }

decls_opt:
    /* empty */ { [] }
  | decls       { $1 }

decls:
    decl       { [$1] }
  | decls decl { $2 :: $1 }

decl:
    template      { $1 }
  | func          { $1 }
  | var SEMICOLON { $1 }

match_:
    MATCH expr match_block { Match($2, $3) }

match_block:
    LBRACE match_arms RBRACE { List.rev $2 }

match_arms:
    match_arm            { [$1]     }
  | match_arms match_arm { $2 :: $1 }

match_arm:
    expr ARM block  { Arm($1, $3) }

conditional:
    IF expr block ELSE block       { If($2, $3, $5) }
/*  | IF expr block ELSE conditional { If($2, $3, $5) }*/
/*  | IF expr block                  { If($2, $3, ()) }*/

for_:
    FOR expr IN expr block { For($2, $4, $5) }

expr:
    INT    { LInt($1)    }
  | FLOAT  { LFloat($1)  }
  | STRING { LString($1) }

  | expr PLUS   expr { Binop($1, Plus,   $3) }
  | expr MINUS  expr { Binop($1, Minus,  $3) }
  | expr TIMES  expr { Binop($1, Times,  $3) }
  | expr DIV    expr { Binop($1, Div,    $3) }
  | expr REM    expr { Binop($1, Rem,    $3) }
  | expr LSHIFT expr { Binop($1, LShift, $3) }
  | expr RSHIFT expr { Binop($1, RShift, $3) }
  | expr BWOR   expr { Binop($1, BwOr,   $3) }
  | expr BWAND  expr { Binop($1, BwAnd,  $3) }
  | expr AND    expr { Binop($1, And,    $3) }
  | expr OR     expr { Binop($1, Or,     $3) }
  | expr LT     expr { Binop($1, Lt,     $3) }
  | expr LTEQ   expr { Binop($1, LtEq,   $3) }
  | expr EQ     expr { Binop($1, Eq,     $3) }
  | expr NEQ    expr { Binop($1, NEq,    $3) }
  | expr GTEQ   expr { Binop($1, GtEq,   $3) }
  | expr GT     expr { Binop($1, Gt,     $3) }
  |      BWNOT  expr { Unop(BwNot, $2) }
  |      NOT    expr { Unop(Not,   $2) }

  | expr LBRACK expr RBRACK { Binop($1, Subscr, $3) }
  | expr DOT expr           { Binop($1, Access, $3) }

  | LPAREN expr RPAREN { $2 }
  | match_      { $1 }
  | conditional { $1 }
  | for_        { $1 }

/*  | typename         { $1 }*/
  | id               { EId($1) }

block_line:
    expr SEMICOLON { Expr($1) }
  | decl  { BDecl($1) }

block_lines:
    block_line             { [$1] }
  | block_lines block_line { $2 :: $1 }

block:
    LBRACE block_lines RBRACE { Block(List.rev $2) }
  | LBRACE RBRACE             { Block([])          }

parse:
    PARSE block { Parse($2) }

program:
    decls_opt parse EOF { Program(List.rev $1, $2) }

