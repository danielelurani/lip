open Toylexer.Main
open Toylexer.Token;;

let%test _ = lexer "Hello World" = [ATOK("Hello"); ATOK("World"); EOF]
let%test _ = lexer "aoie" = [BTOK ("aoie"); EOF]
let%test _ = lexer "Bcasz" = [ATOK ("Bcasz"); EOF]
let%test _ = lexer "bCzaZ" = [CTOK ("bCzaZ"); EOF]
let%test _ = lexer "-3.14" = [DTOK ("-3.14"); EOF]
let%test _ = lexer "0x21af ok" = [ETOK("0x21af"); CTOK("ok"); EOF]
let%test _ = frequency 3 [ID("x"); ASSIGN; ID("y"); SEQ; ID("x"); ASSIGN; ID("x"); PLUS; CONST("1")] = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]