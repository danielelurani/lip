open Toyparser.Main

let%test _ = parse "1 + 2 + 3" |> eval = Some 6
let%test _ = parse "1 + 2 + 3 + (1 + 2)" |> eval = Some 9
let%test _ = parse "5 - 3 - 1" |> eval = Some 1
let%test _ = parse "1 + 2 * 3" |> eval = Some 7
let%test _ = parse "5 * 2 / 3" |> eval = Some 3
let%test _ = parse "5 * 2 / 0" |> eval = None
let%test _ = parse "-1 - 2 - -3" |> eval = Some 0
let%test _ = parse "0x01 + 2" |> eval = Some 3
let%test _ = parse "5 * 2 / 0x00" |> eval = None