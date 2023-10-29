open Types
module T = ANSITerminal

let string_of_cell = function
  | New _ | Unsealed Mined | Unsealed (Safe 0) -> "   "
  | Sealed _ -> " X "
  | Unsealed (Safe n) -> " " ^ string_of_int n ^ " "

let attrs_of_cell =
  let open T in
  function
  | Unsealed Mined -> [ red; on_red ]
  | Unsealed _ -> [ blue; on_cyan ]
  | New _ -> [ green; on_green ]
  | Sealed _ -> [ red; on_green ]

let print_cell c = T.printf (attrs_of_cell c) "%s" (string_of_cell c)

let print_field w =
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
      List.iter print_cell r;
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

let print_game_result =
  let open T in
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
