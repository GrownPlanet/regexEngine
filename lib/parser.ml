exception ParseError of string

type t = {
  head : State.t option;
  last : State.t option;
  lexer : Lexer.t;
}

let init lexer = { head = None; last = None; lexer = lexer }

let push state t = 
  let new_last = 
    match t.last with
    | None -> state
    | Some last -> 
        State.set last state;
        state
  in
  let new_head = Option.value ~default:state t.head in
  {
    head = Some new_head;
    last = Some new_last;
    lexer = t.lexer;
  }

let combine_linear t1 t2 =
  let new_t1 = 
    match t2.head with
    | Some head -> push head t1 
    | None -> t1
  in
  { head = new_t1.head; last = t2.last; lexer = t2.lexer }

let combine_split t1 t2 =
  let split = State.Split ({ ptr = t1.head }, { ptr = t2.head }) in
  {
    head = Some split;
    last = t2.last;
    lexer = t2.lexer;
  }

let change_lexer lexer t = { t with lexer = lexer }

let parse_char char new_lexer t parse_next =
  push (State.Atom (char, { ptr = None })) t
  |> change_lexer new_lexer
  |> parse_next

let rec parse_until_right_bracket t =
  let token, lexer = Lexer.next t.lexer in
  match token with
  | Some (Char c) -> parse_char c lexer t parse_until_right_bracket
  | Some RightBracket -> change_lexer lexer t
  | Some LeftBracket -> pasre_left_bracket t lexer parse_until_right_bracket
  | Some Or ->
    let empty = State.Empty ({ ptr = None }) in
    let t2 = 
      init lexer
      |> parse_until_right_bracket
      |> push empty
    in
    let t1 = push empty t in
    combine_split t1 t2
  | None -> raise (ParseError "expected a `)`")
  | _ -> raise (ParseError "not implemented yet")
and pasre_left_bracket t lexer parse_next =
  init lexer
  |> parse_until_right_bracket
  |> combine_linear t
  |> parse_next

let parse lexer =
  let rec parse_intern t =
    let token, lexer = Lexer.next t.lexer in
    match token with
    | None -> push State.Match t |> change_lexer lexer
    | Some (Char c) -> parse_char c lexer t parse_intern
    | Some LeftBracket -> pasre_left_bracket t lexer parse_intern
    | Some Or ->
      let t2 = parse_intern (init lexer) in
      let t1 = 
        match t2.last with
        | Some last -> push last t
        | None -> t
      in
      combine_split t1 t2
    | Some RightBracket -> raise (ParseError "unexpected `)`")
    | _ -> raise (ParseError "not implemented yet")
  in
  let t = parse_intern (init lexer) in
  match t.head with
  | Some head -> head
  | None -> raise (ParseError "unreachable")
