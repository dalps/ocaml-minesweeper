open Types
open ListMat
open Print
module T = ANSITerminal

let parse s =
  let lexbuf = Lexing.from_string s in
  let cmd = Parser.command Lexer.read_token lexbuf in
  cmd

let is_mined = function
  | New Mined | Sealed Mined | Unsealed Mined -> true
  | _ -> false

let is_sealed = function
  | Sealed _ -> true
  | _ -> false

let mined_nb =
  fold_neighbors (fun acc n -> (if is_mined n then 1 else 0) + acc) 0

let sealed_nb =
  fold_neighbors (fun acc n -> (if is_sealed n then 1 else 0) + acc) 0

let unseal1 =
  update (function
    | New cell -> Unsealed cell
    | _ as c -> c)

let rec unseal w i j =
  let w' = unseal1 w i j in
  match peek w i j with
  | New (Safe 0) -> fold_neighborsij unseal w' w' i j
  | _ -> w'

let seal =
  update (function
    | New cell -> Sealed cell
    | _ as c -> c)

let seal_input w i j =
  match peek w i j with
  | New _ -> Ok (seal w i j, Continue)
  | Sealed _ -> Error "already sealed!"
  | Unsealed _ -> Error "cannot seal an unsealed cell!"

(* the game is won is if there aren't any sealed safe cells left *)
let game =
  fold
    (fun acc c ->
      match (acc, c) with
      | Lose, _ | _, Unsealed Mined -> Lose
      | _, (New (Safe _) | Sealed (Safe _)) -> Continue
      | _ -> acc)
    Win

let unseal_input w i j =
  match peek w i j with
  | New Mined -> Ok (unseal1 w i j, Lose)
  | New _ ->
      let w' = unseal w i j in
      Ok (w', game w')
  | Sealed c -> Ok (update (fun _ -> New c) w i j, Continue)
  | Unsealed _ when sealed_nb w i j = mined_nb w i j ->
      let w' = fold_neighborsij unseal w w i j in
      Ok (w', game w')
  | Unsealed _ -> Error "cannot unseal further!"

let blank_field ~height ~width = init ~height ~width (fun _ _ -> New (Safe 0))

let plant_mines ~p w i j =
  let nij = neighborsij w i j in
  mapij
    (fun i' j' _ ->
      if
        Random.int 100 < p
        && (i', j') <> (i, j)
        && List.for_all (( <> ) (i', j')) nij
      then New Mined
      else New (Safe 0))
    w

let gen_field ?(p = 20) ?(height = 3) ?(width = 3) i j =
  let w = blank_field ~height ~width in
  let w = plant_mines ~p w i j in
  mapij
    (fun i j -> function
      | New (Safe _) -> New (Safe (mined_nb w i j))
      | _ as c -> c)
    w

let prompt () =
  print_prompt ();
  read_line () |> parse

let rec loop w =
  let r =
    try
      match prompt () with
      | U (i, j) -> unseal_input w i j
      | S (i, j) -> seal_input w i j
    with
    | Invalid_coordinates -> Error "invalid coordinates"
    | _ -> Error "syntax error"
  in
  Result.fold
    ~ok:(fun (w', g) ->
      display w';
      match g with
      | Continue -> loop w'
      | _ -> print_game_result g)
    ~error:(fun s ->
      print_error s;
      loop w)
    r

(* #TODO: difficulties, arguments, safe spawn, show all other mines as magenta when lose *)
