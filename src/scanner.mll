{ open Parser }

(* integer unsignedness, width and endianess *)
let int_uns  = 'u'?
let int_wid  = '8'|"16"|"32"|"64"
let int_end  = ("le"|"be")?

(* float width *)
let float_wid = "32"|"64"

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let id     = letter(letter|digit)*

let num  = digit
let dot  = '.'
let sign = ['-' '+']
let exp  = ['e' 'E'] sign? num+

(* float_const adapted from my homework 2 *)
let float_const = sign? ((num+ dot? num* exp)|(num* dot num+ exp?)|(num+ dot))
let int_const = sign? num+
let hex_const = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let bin_const = "0b" ['0' '1']+

rule token = parse
    (* Whitespace and comments (ignored) *)
    [' ' '\t' '\r' '\n'] { token lexbuf   }
    | '#'                { comment lexbuf }

    (* Delimiters *)
    | '('  { LPAREN } | ')'  { RPAREN }
    | '{'  { LBRACE } | '}'  { RBRACE }
    | '['  { LBRACK } | '}'  { RBRACK }

    (* Keywords *)
    | "template" { TEMPLATE  } | "parse"  { PARSE  }
    | "func"     { FUNCTION  } | "return" { RETURN }
    | "for"      { FOR       } | "in"     { IN     }
    | "match"    { MATCH     } | "->"     { ARM    }
    | "if"       { IF        } | "else"   { ELSE   }

    | '.' { DOT       } | ','  { COMMA  }
    | ':' { COLON     } | '@'  { AT     }
    | ';' { SEMICOLON } | '='  { ASSIGN }

    (* Arithmetic *)
    | '+' { PLUS  } | '-' { MINUS }
    | '*' { TIMES } | '/' { DIV   }
    | '%' { REM   }

    (* Bitwise *)
    | "<<" { LSHIFT } | ">>" { RSHIFT }
    | '|'  { BWOR   } | '&'  { BWAND  }
    | '~'  { BWNOT  }

    (* Boolean *)
    | "and" { AND } | "or" { OR } | "not" { NOT }

    (* Comparison *)
    | '<'  { LT } | "<=" { LTEQ } | '>' { GT } | ">=" { GTEQ }
    | "==" { EQ } | "!=" { NEQ  }

    (* Builtin Types *)
    | (int_uns as u) "int" (int_wid as w) (int_end as e)
      { INT_T(u, int_of_string w, e) }

    | "float"(float_wid as w) { FLOAT_T(int_of_string w) }

    | "string"  { STRING_T }
    | "Type"    { TYPE_T   } | "Array"    { ARRAY_T    }
    | "Func"    { FUNC_T   } | "Template" { TEMPLATE_T }
    | "None"    { NONE_T   }

    (* Identifier *)
    | id as id  { ID(id) }

    (* Literals *)
    | (int_const | hex_const | bin_const) as i { INT(int_of_string i)     }
    | float_const as f                         { FLOAT(float_of_string f) }
    | '"'  ([^'"']*  as s) '"'                 { STRING(s) }
    | '\'' ([^'\'']* as s) '\''                { STRING(s) }

    | eof   { EOF }

and comment = parse
     '\n' { token lexbuf }
    | eof { EOF }
    | _   { comment lexbuf }
