exception StateError of string

type t =
| Atom of char * t_ptr
| Split of t_ptr * t_ptr
| Empty of t_ptr
| Match
and t_ptr = { mutable ptr : t option }

let set head next =
  match head with
  | Atom (_, a) -> a.ptr <- Some next
  | Empty e -> e.ptr <- Some next
  | Split _ -> raise (StateError "`Split` needs two next pointers")
  | Match -> raise (StateError "`Match doesn't have a next pointer")

let set_split head next1 next2 =
  match head with
  | Split (s1, s2) ->
    s1.ptr <- Some next1;
    s2.ptr <- Some next2
  | _ -> raise (StateError "`set_split` expected a `split`")

let get_next head =
  match head with
  | Atom (_, next) -> next.ptr
  | Empty next -> next.ptr
  | Split _ -> raise (StateError "`Split` has two next pointers")
  | Match -> raise (StateError "`Match doesn't have a next pointer")
