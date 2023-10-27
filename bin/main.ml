module T = ANSITerminal
open Minesweeper.Main

let () =
  Random.self_init ();
  T.erase Screen;
  let w = gen_field () in
  display w;
  loop w
