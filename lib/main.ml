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

let empty_field ~width ~height = init ~width ~height (fun _ _ -> New (Safe 0))

(* Generate a random field and populate it with hints *)
let gen_field ~width ~height ~nbr_mines ~init_i ~init_j =
  let take n l = List.to_seq l |> Seq.take n |> List.of_seq in
  let w = empty_field ~width ~height in
  let init_ns = neighborsij w init_i init_j in
  let candidates =
    enum_coords w
    |> List.filter (fun (i', j') ->
           (i', j') <> (init_i, init_j)
           && List.for_all (( <> ) (i', j')) init_ns)
    |> List.sort (fun _ _ -> List.nth [-1; 1] (Random.int 2))
    |> take nbr_mines
  in
  (* don't reverse apply *)
  let w =
    mapij
      (fun i j _ ->
        if List.mem (i, j) candidates then New Mined else New (Safe 0))
      w
  in
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
      | _ -> print_game_result w' g)
    ~error:(fun s ->
      print_error s;
      loop w)
    r

(* #TODO: plant a certain number of mines; mine counter *)
