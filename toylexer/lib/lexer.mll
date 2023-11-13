{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let lvowel = ['a''e''i''o''u']

let vowel = ['a''e''i''o''u''A''E''I''O''U']

let cons = letter # vowel

let atok = ['A'-'Z'] chr*

let btok = lvowel+

let ctok = cons* vowel? cons*

let digit = ['0'-'9']

let frac = '.' digit* (* .0001, .123, . *)

let dtok = '-'? num frac?

let hexdigit = ['0'-'9''a'-'f''A'-'F']

let etok = '0' ['x''X'] (hexdigit hexdigit)+


rule read_token =
  parse
  | white { read_token lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf) } 
  | btok { BTOK (Lexing.lexeme lexbuf) } 
  | ctok { CTOK (Lexing.lexeme lexbuf) } 
  | dtok { DTOK (Lexing.lexeme lexbuf) } 
  | etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
