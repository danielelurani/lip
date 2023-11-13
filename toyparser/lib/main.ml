open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

let string_of_result (n : int option) =
  match n with None -> "Illegal expression" | Some n -> string_of_int n

(* eval : ast -> result *)

let eval1 (v1 : int option) (v2 : int option) (op : int -> int -> int) : int option =
  match (v1, v2) with
  | Some n1, Some n2 -> Some (op n1 n2)
  | None, _ | _, None -> None

let rec eval (ast : ast) : int option =
  match ast with
  | Const n -> Some n
  | Add (e1, e2) -> eval1 (eval e1) (eval e2) ( + )
  | Sub (e1, e2) -> eval1 (eval e1) (eval e2) ( - )
  | Mul (e1, e2) -> eval1 (eval e1) (eval e2) ( * )
  | Div (_, e2) when eval e2 = Some 0 -> None
  | Div (e1, e2) -> eval1 (eval e1) (eval e2) ( / )
