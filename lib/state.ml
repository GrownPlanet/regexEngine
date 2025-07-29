exception StateError of string

type t =
| Atom of char * t_ptr
| Split of t_ptr * t_ptr
| Empty of t_ptr
| Match
and t_ptr = { mutable ptr : t option }

let set head next =
  match head with
  | Atom (_, a_ptr) -> a_ptr.ptr <- Some next
  | Empty e -> e.ptr <- Some next
  | Split _ -> raise (StateError "`Split` needs two next pointers")
  | Match -> raise (StateError "`Match doesn't have a next pointer")

let set_split head next1 next2 =
  match head with
  | Split _ -> Split (next1, next2)
  | _ -> raise (StateError "expected a `Split` in `set_split`")

let rec pp ?(indent=0) t =
  let padding = String.make indent ' ' in
  match t with
  | Atom (c, next) ->
      print_string (padding ^ "Char: ");
      print_char c;
      print_newline ();
      (match next.ptr with
      | Some t -> pp ~indent:indent t
      | None -> ())
  | Split (next1, next2) ->
      print_endline (padding ^ "Split: ");
      (match next1.ptr with
      | Some t -> pp ~indent:(indent + 2) t
      | None -> ());
      print_endline (padding ^ "or");
      (match next2.ptr with
      | Some t -> pp ~indent:(indent + 2) t
      | None -> ())
  | Empty (next) ->
      print_endline (padding ^ "Empty");
      (match next.ptr with
      | Some t -> pp ~indent:indent t
      | None -> ())
  | Match ->
      print_endline (padding ^ "Match");
