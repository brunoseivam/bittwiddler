%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token TEMPLATE PARSE FUNCTION RETURN VAR
%token FOR IN MATCH ARM IF ELSE ELIF
%token DOT COMMA COLON AT DOLLAR SEMICOLON ASSIGN
%token PLUS MINUS TIMES DIV REM
%token LSHIFT RSHIFT BWOR BWAND BWNOT
%token AND OR NOT
%token LT LTEQ EQ NEQ GT GTEQ
%token <string> ID
%token <string> ID_T
%token <string * int * string> INT_T
%token <int> FLOAT_T
%token STRING_T
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
  | ID_T        { TId($1)    }

typeargs_opt:
    /* empty */            { None               }
  | LPAREN typeargs RPAREN { Some (List.rev $2) }

typeargs:
    expr                { [$1]   }
  | typeargs COMMA expr { $3::$1 }

type_:
    typename typeargs_opt                    { ScalarType($1, $2)         }
  | typename typeargs_opt LBRACK RBRACK      { ArrayType($1, $2, None)    }
  | typename typeargs_opt LBRACK expr RBRACK { ArrayType($1, $2, Some $4) }

param:
    id COLON type_ { Param($1, $3) }

params:
    param              { [$1]     }
  | params COMMA param { $3 :: $1 }

params_opt:
    /* empty */          { [] }
  | LPAREN params RPAREN { $2 }

template:
    TEMPLATE ID_T params_opt block { Template($2, List.rev $3, $4) }

func:
    FUNCTION id params_opt COLON type_ block { Func($2, $5, List.rev $3, $6) }

opt_hide:
    /* empty */ { false }
  | AT          { true  }

var:
    VAR opt_hide id   COLON type_ ASSIGN expr SEMICOLON {  Var($2, $3, Some $5, Some $7) }
  | VAR opt_hide id   COLON type_             SEMICOLON {  Var($2, $3, Some $5, None   ) }
  | VAR opt_hide id               ASSIGN expr SEMICOLON {  Var($2, $3, None,    Some $5) }
  | VAR DOLLAR   expr COLON expr  ASSIGN expr SEMICOLON { TVar(    $3, Some $5, Some $7) }
  | VAR DOLLAR   expr COLON expr              SEMICOLON { TVar(    $3, Some $5, None   ) }
  | VAR DOLLAR   expr             ASSIGN expr SEMICOLON { TVar(    $3, None,    Some $5) }

decls_opt:
    /* empty */ { [] }
  | decls       { $1 }

decls:
    decl       { [$1] }
  | decls decl { $2 :: $1 }

decl:
    template { $1 }
  | func     { $1 }
  | var      { $1 }

match_:
    MATCH expr match_block { Match($2, $3) }

match_block:
    LBRACE match_arms RBRACE { List.rev $2 }

match_arms:
    match_arm            { [$1]     }
  | match_arms match_arm { $2 :: $1 }

match_arm:
    expr ARM block  { Arm($1, $3) }


if_:
    IF expr block { [If(Some $2, $3)] }

opt_elseifs:
    /* empty */ { [] }
  | elseifs     { $1 }

elseifs:
    elseif         { [$1]   }
  | elseifs elseif { $2::$1 }

elseif:
    ELIF expr block { If(Some $2, $3) }

opt_else:
    /* empty */     { [] }
  | ELSE block      { [If(None, $2)] }

conditional:
    if_ opt_elseifs opt_else { Cond($1 @ (List.rev $2) @ $3) }

forvars:
    id                { [$1]   }
  | forvars COMMA id  { $3::$1 }

for_:
    FOR forvars IN expr block { For($2, $4, $5) }

expr_list:
    /* empty */          { []     }
  | expr                 { [$1]   }
  | expr_list COMMA expr { $3::$1 }

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

  | id       LPAREN expr_list RPAREN { Call($1, List.rev $3)  }

  | id       { EId($1) }
  | typename { EType($1) }

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

