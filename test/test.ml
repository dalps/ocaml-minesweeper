open Minesweeper.Main
open Minesweeper.Types
open ListMat

let w =
  [
    [ Mined; Safe 1; Safe 0; Safe 0; Safe 0 ];
    [ Safe 1; Safe 1; Safe 1; Safe 1; Safe 1 ];
    [ Safe 0; Safe 1; Safe 2; Mined; Safe 1 ];
    [ Safe 0; Safe 1; Mined; Safe 2; Safe 1 ];
    [ Safe 0; Safe 1; Safe 1; Safe 1; Safe 0 ];
  ]

let%test _ =
  neighborsij w 2 2 =
    [ (1, 1); (1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2); (3, 3) ]

let%test _ = neighborsij w 0 0 = [ (0, 1); (1, 0); (1, 1) ]
let%test _ = neighborsij w (-1) 0 = []
let%test _ = neighborsij w 5 5 = []

let%test _ =
  neighbors w 2 2 = [
      Safe 1; Safe 1; Safe 1;
      Safe 1; Mined;
      Safe 1; Mined; Safe 2
    ]

let%test _ =
  neighbors w 4 3 = [
      Mined; Safe 2; Safe 1;
      Safe 1; Safe 0
    ]

let%test _ =
  neighbors w 0 0 = [
      Safe 1;
      Safe 1; Safe 1
    ]

    
let w = map (fun c -> New c) w

let%test _ = mined_nb w 0 0 = 0 (* excludes mine at i,j *)
let%test _ = mined_nb w 2 2 = 2
let%test _ = mined_nb w 0 5 = 0
let%test _ = mined_nb w 2 3 = 1
let%test _ = mined_nb w 3 2 = 1

let%test _ = unseal w 0 3 = [
  [ New(Mined ); Unsealed(Safe 1); Unsealed(Safe 0); Unsealed(Safe 0); Unsealed(Safe 0) ];
  [ New(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = unseal w 4 4 = [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); Unsealed(Safe 2); Unsealed(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); Unsealed(Safe 1); Unsealed(Safe 0) ];
]

let%test _ = unseal w 2 2 = [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); Unsealed(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = unseal w 2 3 = [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); Unsealed(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = seal w 2 3 = [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); Sealed(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = seal w 5 3 = [
  [ New(Mined ); New(Safe 1); New(Safe 0); New(Safe 0); New(Safe 0) ];
  [ New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 2); New(Mined ); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
  [ New(Safe 0); New(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
]

let%test _ = 
  let w' = seal_input w 0 0 |> Result.get_ok |> fst in 
  let w' = unseal_input w' 0 0 |> Result.get_ok |> fst in 
  w' = w

(* unsealing a mined cell ends the game *)
let%test _ =
  Result.fold
    ~ok:(fun (_, state) -> state = Lose)
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
  let w = unseal w 0 3 in
  let w = seal w 0 0 in
  unseal_input w 0 1 = Ok ([
    [Sealed Mined; Unsealed (Safe 1); Unsealed (Safe 0); Unsealed (Safe 0); Unsealed (Safe 0)];
    [Unsealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1)];
    [New (Safe 0); New (Safe 1); New (Safe 2); New Mined; New (Safe 1)];
    [New (Safe 0); New (Safe 1); New Mined; New (Safe 2); New (Safe 1)];
    [New (Safe 0); New (Safe 1); New (Safe 1); New (Safe 1); New (Safe 0)]
  ], Continue)

let%test _ = 
  let w = unseal w 0 3 in
  let w = seal w 1 0 in
  unseal_input w 0 1 = Ok ([
    [Unsealed Mined; Unsealed (Safe 1); Unsealed (Safe 0); Unsealed (Safe 0); Unsealed (Safe 0)];
    [Sealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1)];
    [New (Safe 0); New (Safe 1); New (Safe 2); New Mined; New (Safe 1)];
    [New (Safe 0); New (Safe 1); New Mined; New (Safe 2); New (Safe 1)];
    [New (Safe 0); New (Safe 1); New (Safe 1); New (Safe 1); New (Safe 0)]
  ], Lose)

let%test _ = 
  let w = unseal w 2 2 in
  let w = seal w 2 3 in
  let w = seal w 3 2 in
  unseal_input w 2 2 = Ok ([
    [New Mined; New (Safe 1); New (Safe 0); New (Safe 0); New (Safe 0)];
    [New (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1); Unsealed (Safe 1); New (Safe 1)];
    [New (Safe 0); Unsealed (Safe 1); Unsealed (Safe 2); Sealed Mined; New (Safe 1)];
    [New (Safe 0); Unsealed (Safe 1); Sealed Mined; Unsealed (Safe 2); New (Safe 1)];
    [New (Safe 0); New (Safe 1); New (Safe 1); New (Safe 1); New (Safe 0)]
  ], Continue)

let%test _ = 
  let w = unseal w 2 2 in
  let w = seal w 2 3 in
  unseal w 2 2 = [
    [New Mined; New (Safe 1); New (Safe 0); New (Safe 0); New (Safe 0)];
    [New (Safe 1); New (Safe 1); New (Safe 1); New (Safe 1); New (Safe 1)];
    [New (Safe 0); New (Safe 1); Unsealed (Safe 2); Sealed Mined; New (Safe 1)];
    [New (Safe 0); New (Safe 1); New Mined; New (Safe 2); New (Safe 1)];
    [New (Safe 0); New (Safe 1); New (Safe 1); New (Safe 1); New (Safe 0)]
  ]

let%test _ = 
  let w = unseal w 2 2 in
  let w = seal w 2 3 in
  unseal_input w 2 2 |> Result.is_error

let%test _ = 
  let w = unseal w 0 3 in
  let w = seal w 0 0 in
  unseal_input w 1 1 = Ok ([
    [ Sealed(Mined ); Unsealed(Safe 1); Unsealed(Safe 0); Unsealed(Safe 0); Unsealed(Safe 0) ];
    [ Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1); Unsealed(Safe 1) ];
    [ Unsealed(Safe 0); Unsealed(Safe 1); Unsealed(Safe 2); New(Mined ); New(Safe 1) ];
    [ Unsealed(Safe 0); Unsealed(Safe 1); New(Mined ); New(Safe 2); New(Safe 1) ];
    [ Unsealed(Safe 0); Unsealed(Safe 1); New(Safe 1); New(Safe 1); New(Safe 0) ];
  ], Continue)

let%test _ =
  let w0 = w in
  let w1 = unseal w0 0 4 in
  let w2 = unseal w1 4 0 in
  let w3 = unseal w2 4 4 in
  let w4 = unseal w3 4 2 in
  let w5 = unseal w4 2 4 in
  let w6 = unseal w5 2 2 in
  List.for_all (fun w -> game w = Continue) [w0;w1;w2;w3;w4;w5] &&
  game w6 = Win 

let%test _ =
  let w0 = w in
  let w1 = unseal w0 0 4 in
  let w2 = unseal w1 4 0 in
  let w3 = unseal w2 4 4 in
  let w4 = unseal w3 4 2 in
  let w5 = unseal w4 2 3 in
  let w6 = unseal w5 2 2 in
  game w6 = Lose 
