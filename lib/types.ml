type cell' = Mined | Safe of int
type cell = New of cell' | Unsealed of cell' | Sealed of cell'

type game_state = Lose | Win | Continue

module ListMat = struct
  exception Invalid_coordinates

  let peek w i j =
    try List.nth (List.nth w i) j with _ -> raise Invalid_coordinates

  let map f = List.map (List.map f)

  let mapij f = List.mapi (fun i -> List.mapi (fun j -> f i j))

  let update i j f = mapij (fun i' j' c -> if i = i' && j = j' then f c else c)

  let fold f b = List.fold_left (List.fold_left f) b
end
