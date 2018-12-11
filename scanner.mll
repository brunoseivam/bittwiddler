{
    open Parser

    let unescape s =
        Scanf.sscanf("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

(* integer unsignedness and width *)
let int_uns  = 'u'?
let int_wid  = '8'|"16"|"32"|"64"

(* float width *)
let float_wid = "32"|"64"

let digit  = ['0'-'9']
let ucase  = ['A'-'Z']
let lcase  = ['a'-'z']
let letter = ['A'-'Z' 'a'-'z' '_']
let id     = (lcase | '_')(letter|digit)*
let type_  = (ucase)(letter|digit)*

let num  = digit
let dot  = '.'
let sign = ['-' '+']
let exp  = ['e' 'E'] sign? num+

(* float_lit adapted from my homework 2 *)
let float_lit = sign? ((num+ dot? num* exp)|(num* dot num+ exp?)|(num+ dot))
let int_lit = sign? num+
let hex_lit = sign?"0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let bin_lit = sign?"0b" ['0' '1']+

(* String literal parsing copied, with modifications, from the DECAF
 * project (Spring 2017 *)
let ascii_dquote = [' '-'!' '#'-'[' ']'-'~'] (* ascii without double quote *)
let ascii_squote = [' '-'&' '('-'[' ']'-'~'] (* ascii without single quote *)
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']

rule token = parse
    (* Whitespace and comments (ignored) *)
    [' ' '\t' '\r' '\n'] { token lexbuf   }
    | '#'                { comment lexbuf }

    (* Delimiters *)
    | '('  { LPAREN } | ')'  { RPAREN }
    | '{'  { LBRACE } | '}'  { RBRACE }
    | '['  { LBRACK } | ']'  { RBRACK }

    (* Keywords *)
    | "template" { TEMPLATE  } | "main"   { MAIN  }
    | "func"     { FUNCTION  } | "return" { RETURN }
    | "for"      { FOR       } | "in"     { IN     }
    | "while"    { WHILE     }
    | "match"    { MATCH     } | "->"     { ARM    }
    | "if"       { IF        } | "else"   { ELSE   }
    | "var"      { VAR       } | "elif"   { ELIF   }

    | '.' { DOT       } | ','  { COMMA    }
    | ':' { COLON     } | '_'  { WILDCARD }
    | ';' { SEMICOLON } | '='  { ASSIGN   }

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
    | (int_uns as u) "int" (int_wid as w)
      { INT_T(u="u", int_of_string w) }

    | "float"(float_wid as w) { FLOAT_T(int_of_string w) }
    | "bool"    { BOOL_T   }
    | "string"  { STRING_T }
    | "None"    { NONE_T   }

    (* Literals *)
    | (int_lit | hex_lit | bin_lit) as i    { INT(int_of_string i)     }
    | float_lit as f                        { FLOAT(float_of_string f) }
    | "true"                                { BOOL(true)         }
    | "false"                               { BOOL(false)        }
    | '"' ((ascii_dquote|escape)* as s) '"' { STRING(unescape s) }
    | ''' ((ascii_squote|escape)* as s) ''' { STRING(unescape s) }

    (* Identifier *)
    | id as id  { ID(id) }

    (* Type Identifier *)
    | type_ as type_ { ID_T(type_) }

    | eof   { EOF }

and comment = parse
     '\n' { token lexbuf }
    | eof { EOF }
    | _   { comment lexbuf }
