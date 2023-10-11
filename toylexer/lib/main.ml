open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* inc : 'a -> ('a * int) list -> ('a * int) list *)
let rec inc t = function
    [] -> [(t,1)]
  | (t',n)::fl when t'=t -> (t',n+1)::fl
  | x::fl -> x::(inc t fl)

(* pref: int -> 'a list -> 'a list *)
let rec pref n l = match l with
    [] -> []
  | _ when n=0 -> []
  | x::l' -> x::(pref (n-1) l')
  
(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tl = pref n (List.sort (fun (_,n) (_,n') -> compare n' n) (List.fold_left (fun fl t -> inc t fl) [] tl))
