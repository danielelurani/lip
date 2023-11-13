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


let rec inc (dict : (token * int) list) (t : token) : (token * int) list =
  match dict with
  | [] -> [(t,1)]
  | (t',n)::dict' when t'=t -> (t,n+1)::dict'
  | x::dict' -> x::(inc dict' t)

let frequency1 tokenlist = List.fold_left inc [] tokenlist

let rec take n = function
| [] -> []
| _ when n=0 -> []
| x::t -> x::(take (n-1) t)

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tokenlist = frequency1 tokenlist
  |> List.sort (fun (_,n1) (_,n2) -> compare n2 n1)
  |> take n
