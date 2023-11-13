{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let hexdigit = ['0'-'9''a'-'f''A'-'F']
let hex = '0' ('x'|'X') hexdigit*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | hex | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
