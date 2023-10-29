open Minesweeper.Main
open Minesweeper.Print

let () =
  Random.self_init ();
  let w = gen_field () in
  display w;
  loop w
