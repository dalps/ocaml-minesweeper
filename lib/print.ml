open Types
module T = ANSITerminal

let string_of_cell = function
  | New _ | Unsealed Mined | Unsealed (Safe 0) -> " "
  | Sealed _ -> "X"
  | Unsealed (Safe n) -> string_of_int n

let attrs_of_cell =
  let open T in
  function
  | Unsealed Mined -> [ red; on_red ]
  | Unsealed _ -> [ blue; on_cyan ]
  | New _ -> [ green; on_green ]
  | Sealed _ -> [ red; on_green ]

let print_cell c = T.printf (attrs_of_cell c) " %s " (string_of_cell c)

let attrs_of_cell_game_over =
  let open T in
  function
  | New Mined | Sealed Mined -> [ red; on_magenta ]
  | _ as c -> attrs_of_cell c

let print_cell_game_over c =
  T.printf (attrs_of_cell_game_over c) " %s " (string_of_cell c)

let print_field ?(pp_cell = print_cell) w =
  let r0 = List.nth w 0 in
  let print_hlegend () =
    print_string "   ";
    List.iteri (fun i _ -> Printf.printf " %2i" i) r0;
    print_newline ()
  in
  print_hlegend ();
  List.iteri
    (fun i r ->
      Printf.printf "%3i " i;
      List.iter pp_cell r;
      Printf.printf " %-3i\n" i)
    w;
  print_hlegend ()

let print_game_over w =
  let r0 = List.nth w 0 in
  let print_hlegend () =
    print_string "   ";
    List.iteri (fun i _ -> print_string (" " ^ string_of_int i ^ " ")) r0;
    print_newline ()
  in
  print_hlegend ();
  List.iteri
    (fun i r ->
      print_string (" " ^ string_of_int i ^ " ");
      List.iter
        (function
          | Unsealed Mined as c ->
              T.printf [ T.red; T.on_red ] "%s" (string_of_cell c)
          | (Sealed Mined | New Mined) as c ->
              T.printf [ T.magenta; T.on_magenta ] "%s" (string_of_cell c)
          | _ as c -> print_cell c)
        r;
      print_string (" " ^ string_of_int i ^ " ");
      print_newline ())
    w;
  print_hlegend ()

let print_prompt () =
  let open T in
  print_string [ Bold ] "{";
  print_string [ Bold; Underlined ] "s";
  print_string [ Bold ] "eal|";
  print_string [ Bold; Underlined ] "u";
  print_string [ Bold ] "nseal}x,y > "

let print_game_result w =
  let open T in
  T.erase Screen;
  T.set_cursor 1 1;
  print_field w ~pp_cell:print_cell_game_over;
  function
  | Lose -> print_string [ Bold; red ] "GAME OVER\n"
  | Win -> print_string [ Bold; green ] "FIELD CLEARED!\n"
  | _ -> ()

let print_error s =
  let open T in
  print_string [ yellow ] (s ^ "\n")

let display w =
  T.erase Screen;
  T.set_cursor 1 1;
  print_field w
