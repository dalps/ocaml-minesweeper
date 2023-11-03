open Minesweeper.Main
open Minesweeper.Print
open Minesweeper.Types
open ListMat

let () =
  Random.self_init ();
  let m = 10 in
  let n = 10 in
  let p = 20 in
  let w0 = blank_field m n in
  let rec preamble w =
    let r =
      try
        match prompt () with
        | U (i, j) | S (i, j) ->
            let w = gen_field ~p ~height:m ~width:n i j in
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
