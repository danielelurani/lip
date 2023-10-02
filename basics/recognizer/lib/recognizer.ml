let rec only01 = function
    [] -> true
  | '0'::l -> only01 l
  | '1'::l -> only01 l
  | _ -> false


(* 0?1* *)

let step1 q a = match q with
    0 when a='0' || a='1' -> 1
  | 1 when a='1' -> 1
  | _ -> -1
    
let lang1 w = match List.fold_left step1 0 w with
    0 | 1 -> true
  | _ -> false


(* 0[01]*0 *)

let step2 q a = match q with
    0 when a='0' -> 1
  | 1 when a='0' -> 2
  | 1 when a='1' -> 1
  | 2 when a='0' -> 2
  | 2 when a='1' -> 1    
  | _ -> -1

let lang2 w = match List.fold_left step2 0 w with
    2 -> true
  | _ -> false

(* [01]*0[01]{2} *)
let lang3 w = only01 w

(* 0*10*10*10* *)
let lang4 w = only01 w

(* (00+11)+ *)
let lang5 w = only01 w
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
