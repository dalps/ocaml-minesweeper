type cell' = Mined | Safe of int
type cell = New of cell' | Unsealed of cell' | Sealed of cell'

type game_state = Lose | Win | Continue

type command = U of int * int | S of int * int

module ListMat = struct
  exception Invalid_coordinates

  let valid_coords m n i j = i >= 0 && j >= 0 && i < m && j < n

  let init ~width ~height f =
    List.init height (fun i -> List.init width (fun j -> f i j))

  let peek w i j =
    try List.nth (List.nth w i) j with _ -> raise Invalid_coordinates

  let map f = List.map (List.map f)

  let mapij f = List.mapi (fun i -> List.mapi (fun j -> f i j))

  let update f w i j =
    mapij (fun i' j' c -> if i = i' && j = j' then f c else c) w

  let fold f b = List.fold_left (List.fold_left f) b

  let neighborsij w i j =
    let m = List.length w in
    let n = List.nth w 0 |> List.length in
    if valid_coords m n i j then
      List.filter
        (fun (i, j) -> valid_coords m n i j)
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
    List.map (fun (i', j') -> peek w i' j') (neighborsij w i j)

  let fold_neighbors f b w i j = neighbors w i j |> List.fold_left f b

  let fold_neighborsij f b w i j =
    let ns = neighborsij w i j in
    let rec helper f accu = function
      | [] -> accu
      | (i', j') :: t -> helper f (f accu i' j') t
    in
    helper f b ns
end
