(* Run with: dune exec -- minesweeper [options] *)

open Minesweeper.Main
open Minesweeper.Print
open Minesweeper.Types
open ListMat

let usage_msg = "dune exec minesweeper [-w <width>] [-h <height>] [-m <nbr_mines>]"

let width = ref 10
let height = ref 10
let mines = ref 20

let anon_fun = print_endline

let speclist =
  [
    ("-w", Arg.Set_int width, "Width of the play field");
    ("-h", Arg.Set_int height, "Height of the play field");
    ("-m", Arg.Set_int mines, "Number of mines to hide in the play field");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  Random.self_init ();
  let w0 = blank_field ~width:!width ~height:!height in
  let rec preamble w =
    let r =
      try
        match prompt () with
        | U (i, j) | S (i, j) ->
            let w = gen_field ~p:!mines ~width:!width ~height:!height i j in
            unseal_input w i j
      with
      | Invalid_coordinates -> Error "invalid coordinates"
      | _ -> Error "syntax error"
    in
    Result.fold
      ~ok:(fun p -> p)
      ~error:(fun s ->
        print_error s;
        preamble w)
      r
  in
  display w0;
  match preamble w0 with
  | w1, Continue ->
      display w1;
      loop w1
  | w1, g ->
      display w1;
      print_game_result g
