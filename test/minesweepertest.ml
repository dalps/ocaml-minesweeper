open Minesweeper.Main

let w =
  [
    [ Mined; Safe 1; Safe 0; Safe 0; Safe 0 ];
    [ Safe 1; Safe 1; Safe 1; Safe 1; Safe 1 ];
    [ Safe 0; Safe 1; Safe 2; Mined; Safe 1 ];
    [ Safe 0; Safe 1; Mined; Safe 2; Safe 1 ];
    [ Safe 0; Safe 1; Safe 1; Safe 1; Safe 0 ];
  ]

let%test _ =
  neighbors_pos w 2 2
  = [ (1, 1); (1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2); (3, 3) ]

let%test _ = neighbors_pos w 0 0 = [ (0, 1); (1, 0); (1, 1) ]
let%test _ = neighbors_pos w (-1) 0 = []
let%test _ = neighbors_pos w 5 5 = []

let%test _ =
  neighbors w 2 2
  = [
      [ Some (Safe 1); Some (Safe 1); Some (Safe 1) ];
      [ Some (Safe 1); Some (Safe 2); Some Mined ];
      [ Some (Safe 1); Some Mined; Some (Safe 2) ];
    ]

let%test _ =
  neighbors w 4 3
  = [
      [ Some Mined; Some (Safe 2); Some (Safe 1) ];
      [ Some (Safe 1); Some (Safe 1); Some (Safe 0) ];
      [ None; None; None ];
    ]

let%test _ =
  neighbors w 0 0
  = [
      [ None; None; None ];
      [ None; Some Mined; Some (Safe 1) ];
      [ None; Some (Safe 1); Some (Safe 1) ];
    ]

let%test _ = bomb_nb w 0 0 = 0 (* excludes bomb at i,j *)
let%test _ = bomb_nb w 2 2 = 2
let%test _ = bomb_nb w 0 5 = 0
let%test _ = bomb_nb w 2 3 = 1
let%test _ = bomb_nb w 3 2 = 1
let%test _ = bomb_nb w 2 2 = 2
let w = update (fun c -> New c) w

let%test _ = unseal w 0 3 =   [
  [ New(Mined ); Unsealed(Safe 1); Unsealed(Safe 0); Unsealed(Safe 0); Unsealed(Safe 0) ];
  [ New(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = unseal w 4 4 =   [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); Unsealed(Safe 2); Unsealed(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); Unsealed(Safe 1); Unsealed(Safe 0) ];
]

let%test _ = unseal w 2 2 =   [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); Unsealed(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = unseal w 2 3 =   [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); Unsealed(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = seal w 2 3 =   [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); Sealed(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = seal w 5 3 =   [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = unseal (seal w 0 0) 0 0 = w

(* unsealing a mined cell ends the game *)
let%test _ =
  Result.fold
    ~ok:(fun (_, state) -> state = Over)
    ~error:(fun _ -> false)
    (unseal_input w 0 0)

(* unsealing a flagged mined cell does not end the game *)
let%test _ =
  let w = seal w 0 0 in
  Result.fold
    ~ok:(fun (_, state) -> state = Continue)
    ~error:(fun _ -> false)
    (unseal_input w 0 0)

let%test _ =
  let w0 = w in
  let w1 = unseal w0 0 4 in
  let w2 = unseal w1 4 0 in
  let w3 = unseal w2 4 4 in
  let w4 = unseal w3 4 2 in
  let w5 = unseal w4 2 4 in
  let w6 = unseal w5 2 2 in
  List.for_all (fun w -> win w = false) [w0;w1;w2;w3;w4;w5] &&
  win w6 = true
