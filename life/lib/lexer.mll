{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }
  | ['E''e'] { E }
  | ['S''s'] { S }
  | ['B''b'] { B }
  | '/' { SLASH }
  | ',' { COMMA }
  | ".." { RANGE }
  | num { NUM (Lexing.lexeme lexbuf) }
  | eof { EOF }
