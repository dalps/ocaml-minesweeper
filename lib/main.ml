module T = ANSITerminal

type cell = Bomb | Free of int
type world = cell list list

let string_of_cell = function
  | Bomb -> "   "
  | Free 0 -> "   "
  | Free n -> " " ^ string_of_int n ^ " "

let string_of_world w =
  List.fold_left
    (fun s r ->
      s ^ "\n" ^ List.fold_left (fun s c -> s ^ string_of_cell c) "" r)
    "" w

let is_bomb = function Bomb -> true | _ -> false

let peek (w : world) i j =
  match (i, j) with
  | _ when i < 0 || j < 0 -> None
  | _ -> (
      let row = List.nth_opt w i in
      match row with None -> None | Some r -> List.nth_opt r j)

let neighbors w i j =
  [
    [ peek w (i - 1) (j - 1); peek w (i - 1) j; peek w (i - 1) (j + 1) ];
    [ peek w i (j - 1); peek w i j; peek w i (j + 1) ];
    [ peek w (i + 1) (j - 1); peek w (i + 1) j; peek w (i + 1) (j + 1) ];
  ]

let neighbors_flat w i j =
  [
    peek w (i - 1) (j - 1);
    peek w (i - 1) j;
    peek w (i - 1) (j + 1);
    peek w i (j - 1);
    peek w i j;
    peek w i (j + 1);
    peek w (i + 1) (j - 1);
    peek w (i + 1) j;
    peek w (i + 1) (j + 1);
  ]

let bomb_nb w i j =
  let ns = neighbors_flat w i j in
  List.fold_left
    (fun acc n ->
      Option.fold ~none:0 ~some:(fun cell -> if is_bomb cell then 1 else 0) n
      + acc)
    0 ns

let rnd_world ~p ~height ~width =
  List.init height (fun _ ->
      List.init width (fun _ -> if Random.int 100 < p then Bomb else Free 0))

let step w =
  List.mapi
    (fun i r ->
      List.mapi
        (fun j -> function Bomb -> Bomb | Free _ -> Free (bomb_nb w i j))
        r)
    w

let gen_world ?(p = 5) ?(height = 20) ?(width = 20) () =
  let w = rnd_world ~p ~height ~width in
  step w

let attrs_of_cell =
  let open T in
  function
  | Bomb -> [ red; on_red ] | Free 0 -> [ on_cyan ] | Free _ -> [ blue; on_cyan ]

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
