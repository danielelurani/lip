open Life.Main

let%test _ = parse "S23/B3" = ([23],[3])
let%test _ = parse "ES2,3/B3" = ([2;3],[3])
let%test _ = parse "E2,3/3" = ([2;3],[3])
let%test _ = parse "E 2,3 / 3" = ([2;3],[3])
let%test _ = parse "ES0..5/B7..12" = ([0;1;2;3;4;5],[7;8;9;10;11;12])
let%test _ = parse "ES0..5,7..12/B" = ([0;1;2;3;4;5;7;8;9;10;11;12],[])
let%test _ = parse "ES0..5,6,7..12/B" = ([0;1;2;3;4;5;6;7;8;9;10;11;12],[])
let%test _ = parse "ES0..5,7..12/B6" = ([0;1;2;3;4;5;7;8;9;10;11;12],[6])