module T = ANSITerminal
open Life.Main

let _ = match Array.length(Sys.argv) with
    3 -> let k = int_of_string (Sys.argv.(2)) in
    let r = parse (Sys.argv.(1)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k r in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life SB_rule n_rounds"
