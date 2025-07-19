exception ParseError of string

type state =
| Atom of char * state_ptr
| Split of state_ptr * state_ptr
| Empty of state_ptr
| Match
and state_ptr = { (* mutable *) ptr : state option }

let rec parse lexer =
  match Lexer.next lexer with
  | (None, lexer) -> (Match, lexer)
  | (Some (Char c), lexer) ->
      let (tail, lexer) = parse lexer in
      let ptr = { ptr = Some tail } in
      (Atom (c, ptr), lexer)
  | _ -> raise (ParseError "not implemented yet")

let rec pp ?(indent=0) state =
  let padding = String.make indent ' ' in
  match state with
  | Atom (c, next) ->
      begin
      print_string (padding ^ "Char: ");
      print_char c;
      print_newline ();
      match next.ptr with
      | Some state -> pp ~indent:indent state
      | None -> ()
      end
  | Split (next1, next2) ->
      begin
      match next1.ptr with
      | Some state -> pp ~indent:indent state
      | None -> ()
      end
      begin
      match next2.ptr with
      | Some state -> pp ~indent:indent state
      | None -> ()
      end
  | _ -> raise (ParseError "not implemented yet")
