type t =
  | Char of char
  | LeftBracket
  | RightBracket
  | Or
  (* TODO *)
  | Question
  | Star
  | Plus

let pp t =
  match t with
  | Char c ->
      print_string "Char: ";
      print_char c;
      print_newline ()
  | LeftBracket -> print_endline "LeftBracket"
  | RightBracket -> print_endline "RightBracket"
  | Or -> print_endline "Or"
  | Question -> print_endline "Question"
  | Star -> print_endline "Star"
  | Plus -> print_endline "Plus"
