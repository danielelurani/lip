(* tokens *)
type token = A | B | X

(* convert a string to a list of char *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s =
  let tok_of_char c = match c with
    'A' -> A
    | 'B' -> B
    | '=' -> X
    | _ -> failwith "wrong character"
  in List.map tok_of_char (explode s)

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l =
  let rec step s = function
    [] -> s
    | A::l when s=0 -> step 0 l
    | X::l when s=0 || s=1 -> step 1 l
    | B::l when s=1 || s=2 -> step 2 l
    | _ -> 3
  in (step 0 l <> 3)

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l =
  let rec step nA nB = function 
    [] -> (nA,nB)
    | A::l -> step (nA+1) nB l
    | B::l -> step nA (nB+1) l
    | X::l -> step nA nB l
  in let (nA,nB) = step 0 0 l in
  if nA>nB then A else if nA<nB then B else X

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
    A -> "the winner is A"
  | B -> "the winner is B"
  | _ -> "no winner"
