exception ParseError of string

type t = {
  head : State.t option;
  previous : State.t option;
  last : State.t option;
  lexer : Lexer.t;
}

let init lexer = { head = None; previous = None; last = None; lexer }

let push state t =
  (match t.last with
    | Some last -> State.set last state
    | None -> ());
  let new_head = Some (Option.value ~default:state t.head) in
  { head = new_head; previous = t.last; last = Some state; lexer = t.lexer }

let combine_linear t1 t2 =
  let new_t1 =
    match t2.head with
    | Some head -> push head t1
    | None -> t1
  in
  { head = new_t1.head; previous = t1.last; last = t2.last; lexer = t2.lexer }

let combine_split t1 t2 =
  let split = Some (State.Split ({ ptr = t1.head }, { ptr = t2.head })) in
  { head = split; previous = t1.last; last = t2.last; lexer = t2.lexer }

let change_lexer lexer t = { t with lexer }

let rec parse_internal lexer =
  let t = init lexer in
  parse_or t

and parse_or t =
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some Token.Pipe ->
    let empty = State.Empty { ptr = None } in
    let t1 = push empty t in
    let t2 = parse_internal lexer |> push empty in
    combine_split t1 t2
  | _ -> parse_mult t

and parse_mult t =
  let get_first t =
    match t.previous with
    | Some previous -> Option.get (State.get_next previous)
    | None -> Option.get t.head
  in
  let update_last t state =
    match t.last with
    | Some last -> State.set last state
    | None -> raise (ParseError "multiplier requires a preceding expression")
  in
  let make_state t lexer split last =
    match t.previous with
    | Some _ ->
      push split { t with last = t.previous; lexer }
      |> fun result -> { result with last = Some last }
    | None ->
      { head = Some split; previous = None; last = Some last; lexer = lexer }
  in
  let get_first_last_split t =
    let first = get_first t in
    let last = State.Empty { ptr = None } in
    let split = State.Split ({ ptr = Some last }, { ptr = Some first }) in
    (first, last, split)
  in
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some Question -> 
    let (_, last, split) = get_first_last_split t in
    update_last t last;
    make_state t lexer split last |> parse_or
  | Some Star ->
    let (_, last, split) = get_first_last_split t in
    update_last t split;
    make_state t lexer split last |> parse_or
  | Some Plus ->
    let (first, last, split) = get_first_last_split t in
    update_last t split;
    make_state t lexer first last |> parse_or
  | _ -> parse_atom t

and parse_atom t =
  let handle_atom t lexer state =
    let atom = State.Atom (state, { ptr = None }) in
    push atom t |> change_lexer lexer |> parse_or
  in
  let rec parse_character_class acc t =
    let next, lexer = Lexer.next_char_class t.lexer in
    match next with
    | Some Token.RightBracket -> (change_lexer lexer t, acc)
    | Some token ->
      change_lexer lexer t |> parse_character_class (token :: acc)
    | None -> raise (ParseError "expected a ']' after a '['")
  in
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some Token.Char ch -> handle_atom t lexer (State.Char ch)
  | Some Token.WildCard -> handle_atom t lexer State.WildCard
  | Some Token.Digit -> handle_atom t lexer State.Digit
  | Some Token.Whitespace -> handle_atom t lexer State.Whitespace
  | Some Token.Word -> handle_atom t lexer State.Word
  | Some Token.LeftParenthese ->
    let new_t = parse_internal lexer in
    let next, lexer = Lexer.next new_t.lexer in
    if next = Some Token.RightParenthese then
      combine_linear t new_t |> change_lexer lexer |> parse_or
    else
      raise (ParseError "expected ')' after '('")
  | Some Token.RightParenthese -> t
  | Some Token.LeftBracket ->
    let t, list = change_lexer lexer t |> parse_character_class [] in
    let atom = State.Atom ((State.CharClass list), { ptr = None }) in
    push atom t |> parse_or
  | Some Token.RightBracket -> t
  | None -> change_lexer lexer t
  | _ -> parse_or t

let parse lexer =
  let t = parse_internal lexer |> push State.Match in
  Option.get t.head
