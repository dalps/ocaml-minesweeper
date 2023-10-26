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