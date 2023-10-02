(* [01]+ *)

let rec lang1 = function
    [] -> false
  | ['0'] | ['1'] -> true
  | '0'::l -> lang1 l
  | '1'::l -> lang1 l
  | _ -> false

(* 0?1* *)

let step2 q a = match q with
    0 when a='0' || a='1' -> 1
  | 1 when a='1' -> 1
  | _ -> -1
    
let lang2 w = match List.fold_left step2 0 w with
    0 | 1 -> true
  | _ -> false


(* 0[01]*0 *)

let step3 q a = match q with
    0 when a='0' -> 1
  | 1 when a='0' -> 2
  | 1 when a='1' -> 1
  | 2 when a='0' -> 2
  | 2 when a='1' -> 1    
  | _ -> -1

let lang3 w = match List.fold_left step3 0 w with
    2 -> true
  | _ -> false

(* 0*10*10* *)

let step4 q a = match q with
    0 when a='0' -> 0
  | 0 when a='1' -> 1
  | 1 when a='0' -> 1
  | 1 when a='1' -> 2
  | 2 when a='0' -> 2
  | 2 when a='1' -> -1
  | _ -> -1

let lang4 w = match List.fold_left step4 0 w with
    2 -> true
  | _ -> false

let step5 q a = match q with
    0 when a='0' -> 1
  | 0 when a='1' -> 4
  | 1 when a='0' -> 2
  | 2 when a='0' -> 3
  | 2 when a='1' -> 6
  | 3 when a='0' -> 2
  | 4 when a='1' -> 5
  | 5 when a='0' -> 3
  | 5 when a='1' -> 6   
  | 6 when a='1' -> 5   
  | _ -> -1

(* (00|11)+ *)
let lang5 w = match List.fold_left step5 0 w with
    2 | 5 -> true
  | _ -> false

    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
