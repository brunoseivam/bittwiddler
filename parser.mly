%{ open Ast %}

%token WILDCARD
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token TEMPLATE MAIN FUNCTION RETURN VAR
%token FOR IN WHILE MATCH ARM IF ELSE ELIF
%token DOT COMMA COLON SEMICOLON ASSIGN
%token PLUS MINUS TIMES DIV REM
%token LSHIFT RSHIFT BWOR BWAND BWNOT
%token AND OR NOT
%token LT LTEQ EQ NEQ GT GTEQ
%token <string> ID
%token <string> ID_T
%token <bool * int> INT_T
%token <int> FLOAT_T
%token STRING_T
%token BOOL_T
%token NONE_T
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token EOF

%right AT
%right COMMA
%right ASSIGN
%left OR
%left BWOR
%left AND
%left BWAND
%left EQ NEQ
%left LT LTEQ GT GTEQ
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV REM
%right NOT BWNOT
%left DOT LBRACK RBRACK

%start program
%type <Ast.program> program

%%

typename:
    INT_T             { TInt($1)    }
  | FLOAT_T           { TFloat($1)  }
  | STRING_T          { TString     }
  | BOOL_T            { TBool       }
  | ID_T typeargs_opt { TId($1, $2) }
  | NONE_T            { TNone       }

typeargs_opt:
    /* empty */            { None               }
  | LPAREN typeargs RPAREN { Some (List.rev $2) }

typeargs:
    expr                { [$1]   }
  | typeargs COMMA expr { $3::$1 }

type_:
    typename                    { ScalarType($1)         }
  | typename LBRACK RBRACK      { ArrayType($1, None)    }
  | typename LBRACK INT RBRACK  { ArrayType($1, Some $3) }

param:
    ID COLON type_ { Param($1, $3) }

params:
    param              { [$1]     }
  | params COMMA param { $3 :: $1 }

params_opt:
    /* empty */          { [] }
  | LPAREN params RPAREN { $2 }

var:
    VAR ID COLON type_ ASSIGN expr SEMICOLON {  Var($2, Some $4, Some $6) }
  | VAR ID COLON type_             SEMICOLON {  Var($2, Some $4, None   ) }
  | VAR ID             ASSIGN expr SEMICOLON {  Var($2, None,    Some $4) }

match_:
    MATCH expr match_block { Match($2, $3) }

match_block:
    LBRACE match_arms RBRACE { List.rev $2 }

match_arms:
    match_arm            { [$1]     }
  | match_arms match_arm { $2 :: $1 }

match_arm:
    expr ARM block     { (Some $1, $3) }
  | WILDCARD ARM block { (None,    $3) }

if_:
    IF expr block { [(Some $2, $3)] }

opt_elseifs:
    /* empty */ { [] }
  | elseifs     { $1 }

elseifs:
    elseif         { [$1]   }
  | elseifs elseif { $2::$1 }

elseif:
    ELIF expr block { (Some $2, $3) }

opt_else:
    /* empty */     { [] }
  | ELSE block      { [(None, $2)] }

conditional:
    if_ opt_elseifs opt_else { Cond($1 @ (List.rev $2) @ $3) }

forvars:
    ID                { [$1]   }
  | forvars COMMA ID  { $3::$1 }

for_:
    FOR forvars IN expr block { For($2, $4, $5) }

while_:
    WHILE expr block { While($2, $3) }

expr_list:
    /* empty */          { []     }
  | expr                 { [$1]   }
  | expr_list COMMA expr { $3::$1 }

expr:
    INT    { LInt($1)    }
  | FLOAT  { LFloat($1)  }
  | STRING { LString($1) }
  | BOOL   { LBool($1)   }

  | LBRACK expr_list RBRACK { LArray(List.rev $2) }

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
  |      MINUS  expr { Unop(Neg,   $2) }
  |      BWNOT  expr { Unop(BwNot, $2) }
  |      NOT    expr { Unop(Not,   $2) }

  | expr LBRACK expr RBRACK { Binop($1, Subscr, $3) }
  | expr DOT expr           { Binop($1, Access, $3) }
  | expr ASSIGN expr        { Binop($1, Assign, $3) }

  | LPAREN expr RPAREN { $2 }
  | match_      { $1 }
  | conditional { $1 }
  | for_        { $1 }
  | while_      { $1 }

  | ID       LPAREN expr_list RPAREN { Call($1, List.rev $3)  }

  | ID       { Id($1) }
  | typename { EType($1) }

block_stmt:
    expr SEMICOLON        { Expr($1)   }
  | var                   { LVar($1)   }
  | RETURN expr SEMICOLON { Return($2) }

block_stmts:
    block_stmt             { [$1] }
  | block_stmts block_stmt { $2 :: $1 }

block:
    LBRACE block_stmts RBRACE { List.rev $2 }
  | LBRACE RBRACE             { []          }

tblock_stmt:
    var                   { Field($1) }
  | expr SEMICOLON        { TExpr($1) }
  | VAR LBRACK expr RBRACK COLON LBRACK expr RBRACK  ASSIGN expr SEMICOLON
                          { TField( $3, Some $7, Some $10) }
  | VAR LBRACK expr RBRACK COLON LBRACK expr RBRACK              SEMICOLON
                          { TField( $3, Some $7, None   )  }
  | VAR LBRACK expr RBRACK                           ASSIGN expr SEMICOLON
                          { TField( $3, None,    Some $6)  }

tblock_stmts:
    tblock_stmt              { [$1] }
  | tblock_stmts tblock_stmt { $2 :: $1 }

tblock:
    LBRACE tblock_stmts RBRACE { List.rev $2 }
  | LBRACE RBRACE              { []          }

program_decl:
    TEMPLATE ID_T params_opt tblock          { Template($2, List.rev $3, $4) }
  | FUNCTION ID params_opt COLON type_ block { Func($2, $5, List.rev $3, $6) }
  | var                                      { GVar($1)                      }

program_decls_opt:
    /* empty */   { [] }
  | program_decls { $1 }

program_decls:
    program_decl               { [$1] }
  | program_decls program_decl { $2 :: $1 }

program:
    program_decls_opt MAIN block EOF { Program(List.rev $1, $3) }

