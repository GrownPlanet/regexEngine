exception ParseError of string

type t = {
  head : State.t option;
  previous : State.t option;
  last : State.t option;
  lexer : Lexer.t;
}

let init lexer = { head = None; previous = None; last = None; lexer = lexer }

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
  let update_last t state =
    match t.last with
    | Some last -> State.set last state
    | None -> raise (ParseError "multiplier requires a preceding expression")
  in
  let make_next t =
    match t.previous with
    | Some previous -> Option.get (State.get_next previous)
    | None -> Option.get t.head
  in
  let make_state t lexer split empty =
    match t.previous with
    | Some _ ->
      push split { t with last = t.previous; lexer }
      |> fun result -> { result with last = Some empty }
    | None ->
      { head = Some split; previous = None; last = Some empty; lexer = lexer }
  in
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some Question -> 
    let empty = State.Empty { ptr = None } in
    update_last t empty;
    let next = make_next t in
    let split = State.Split ( { ptr = Some empty }, { ptr = Some next }) in
    make_state t lexer split empty |> parse_or
  | Some Star ->
    let split = State.Split ({ ptr = None }, { ptr = None }) in
    let empty = State.Empty { ptr = None } in
    update_last t split;
    let next = make_next t in
    State.set_split split empty next;
    make_state t lexer split empty |> parse_or
  | _ -> parse_atom t

and parse_atom t =
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some Token.Char ch ->
    let atom = State.Atom (ch, { ptr = None }) in
    push atom t |> change_lexer lexer |> parse_or
  | Some Token.LeftBracket ->
    let new_t = parse_internal lexer in
    let next, lexer = Lexer.next new_t.lexer in
    if next = Some Token.RightBracket then
      combine_linear t new_t |> change_lexer lexer |> parse_or
    else
      raise (ParseError "expected `)` after `(`")
  | Some Token.RightBracket -> t
  | None -> change_lexer lexer t
  | _ -> parse_or t

let parse lexer =
  let t = parse_internal lexer |> push State.Match in
  Option.get t.head
