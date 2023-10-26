module T = ANSITerminal

type cell = Mined | Safe of int
type cell_state = New of cell | Unsealed of cell | Sealed of cell
type world = cell list list
type field = cell_state list list
type game_state = Over | Win | Continue

let string_of_cell = function
  | Mined -> "   "
  | Safe 0 -> "   "
  | Safe n -> " " ^ string_of_int n ^ " "

let string_of_world w =
  List.fold_left
    (fun s r ->
      s ^ "\n" ^ List.fold_left (fun s c -> s ^ string_of_cell c) "" r)
    "" w

let is_bomb = function Mined -> true | _ -> false

let peek w i j =
  match (i, j) with
  | _ when i < 0 || j < 0 -> None
  | _ -> (
      let row = List.nth_opt w i in
      match row with None -> None | Some r -> List.nth_opt r j)

let update f w = List.map (List.map f) w
let updateij f w = List.mapi (fun i -> List.mapi (fun j -> f i j)) w

let neighbors w i j =
  [
    [ peek w (i - 1) (j - 1); peek w (i - 1) j; peek w (i - 1) (j + 1) ];
    [ peek w i (j - 1); peek w i j; peek w i (j + 1) ];
    [ peek w (i + 1) (j - 1); peek w (i + 1) j; peek w (i + 1) (j + 1) ];
  ]

let neighbors_pos w i j =
  let r0 = List.nth w 0 in
  let m = List.length w in
  let n = List.length r0 in
  if i >= 0 && j >= 0 && i < m && j < n then
    List.filter
      (fun (i', j') -> i' >= 0 && j' >= 0 && i' < m && j' < n)
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

let neighbors_flat w i j =
  List.map (fun (i', j') -> peek w i' j') (neighbors_pos w i j)

let bomb_nb w i j =
  let ns = neighbors_flat w i j in
  List.fold_left
    (fun acc n ->
      Option.fold ~none:0 ~some:(fun cell -> if is_bomb cell then 1 else 0) n
      + acc)
    0 ns

let unseal1 w i j =
  updateij
    (fun i' j' c ->
      match c with
      | (New cell | Sealed cell) when i = i' && j = j' -> Unsealed cell
      | _ -> c)
    w

let seal w i j =
  updateij
    (fun i' j' c ->
      match c with New cell when i = i' && j = j' -> Sealed cell | _ -> c)
    w

let rec unseal w i j =
  match peek w i j with
  | None | Some (Unsealed _) -> w
  | Some (New cell | Sealed cell) -> (
      let w' = unseal1 w i j in
      let ns = neighbors_pos w i j in
      match cell with
      | Safe 0 -> List.fold_left (fun acc (i', j') -> unseal acc i' j') w' ns
      | _ -> w')

let seal_input w i j =
  match peek w i j with
  | None -> Error "wrong selection"
  | Some (New _) -> Ok (seal w i j, Continue)
  | Some (Sealed _ | Unsealed _) -> Error "cannot seal again!"

let unseal_input w i j =
  match peek w i j with
  | None -> Error "wrong selection"
  | Some (New Mined | Sealed Mined) -> Ok (unseal w i j, Over)
  | Some (New _ | Sealed _) -> Ok (unseal w i j, Continue)
  | Some (Unsealed _) -> Error "cannot unseal again!"

let rnd_world ~p ~height ~width =
  List.init height (fun _ ->
      List.init width (fun _ -> if Random.int 100 < p then Mined else Safe 0))

let step w =
  updateij
    (fun i j -> function Mined -> Mined | Safe _ -> Safe (bomb_nb w i j))
    w

let gen_world ?(p = 5) ?(height = 5) ?(width = 5) () =
  let w = rnd_world ~p ~height ~width in
  step w

let gen_field () =
  let w = gen_world () in
  update (fun c -> New c) w

let attrs_of_cell =
  let open T in
  function
  | Mined -> [ red; on_red ]
  | Safe 0 -> [ on_cyan ]
  | Safe _ -> [ blue; on_cyan ]

let print_cell c = T.printf (attrs_of_cell c) "%s" (string_of_cell c)

let print_world w =
  List.iter
    (fun r ->
      List.iter print_cell r;
      print_newline ())
    w

let display w =
  let open T in
  erase Screen;
  set_cursor 1 1;
  print_world w
