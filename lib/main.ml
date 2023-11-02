open Types
open ListMat
open Print
module T = ANSITerminal

let is_mined = function
  | Mined -> true
  | _ -> false

let is_sealed = function
  | Sealed _ -> true
  | _ -> false

let neighbors_pos w i j =
  let r0 = List.nth w 0 in
  let m = List.length w in
  let n = List.length r0 in
  let is_valid (i', j') = i' >= 0 && j' >= 0 && i' < m && j' < n in
  if is_valid (i, j) then
    List.filter is_valid
      [
        (i - 1, j - 1);
        (i - 1, j);
        (i - 1, j + 1);
        (i, j - 1);
        (i, j + 1);
        (i + 1, j - 1);
        (i + 1, j);
        (i + 1, j + 1);
      ]
  else []

let neighbors w i j =
  List.map (fun (i', j') -> peek w i' j') (neighbors_pos w i j)

let fold_neighbors f b w i j = neighbors w i j |> List.fold_left f b

let mined_nb =
  fold_neighbors (fun acc n -> (if is_mined n then 1 else 0) + acc) 0

let sealed_nb =
  fold_neighbors (fun acc n -> (if is_sealed n then 1 else 0) + acc) 0

let unseal1 w i j =
  update i j
    (function
      | New cell -> Unsealed cell
      | Sealed cell -> New cell
      | _ as c -> c)
    w

let rec unseal w i j =
  let w' = unseal1 w i j in
  match peek w i j with
  | New (Safe 0) ->
      let ns = neighbors_pos w i j in
      List.fold_left (fun acc (i', j') -> unseal acc i' j') w' ns
  | _ -> w'

let seal w i j =
  update i j
    (function
      | New cell -> Sealed cell
      | _ as c -> c)
    w

let seal_input w i j =
  match peek w i j with
  | New _ -> Ok (seal w i j, Continue)
  | Sealed _ -> Error "already sealed!"
  | Unsealed _ -> Error "cannot seal an unsealed cell!"

(* check if there are any sealed safe cells *)
let win w =
  List.for_all
    (List.for_all (function
      | New (Safe _) | Sealed (Safe _) -> false
      | _ -> true))
    w

let unseal_input w i j =
  match peek w i j with
  | New Mined -> Ok (unseal1 w i j, Lose)
  | New _ ->
      let w' = unseal w i j in
      Ok (w', if win w' then Win else Continue)
  | Sealed _ -> Ok (unseal1 w i j, Continue)
  | Unsealed _ -> Error "already unsealed!"

let plant_mines ~p ~height ~width =
  List.init height (fun _ ->
      List.init width (fun _ -> if Random.int 100 < p then Mined else Safe 0))

let gen_field ?(p = 10) ?(height = 10) ?(width = 10) () =
  let w = plant_mines ~p ~height ~width in
  mapij
    (fun i j -> function
      | Mined -> New Mined
      | Safe _ -> New (Safe (mined_nb w i j)))
    w

let rec loop w =
  print_prompt ();
  let choice = read_line () in
  let r =
    try
      let i = String.make 1 choice.[1] |> int_of_string in
      let j = String.make 1 choice.[3] |> int_of_string in
      match choice.[0] with
      | 'U' | 'u' -> unseal_input w i j
      | 'S' | 's' -> seal_input w i j
      | _ -> Error "invalid command"
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
