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
  { head = Some new_head; last = Some new_last; lexer = t.lexer }

let combine_linear t1 t2 =
  let new_t1 = 
    match t2.head with
    | Some head -> push head t1 
    | None -> t1
  in
  { head = new_t1.head; last = t2.last; lexer = t2.lexer }

let combine_split t1 t2 =
  let split = State.Split ({ ptr = t1.head }, { ptr = t2.head }) in
  { head = Some split; last = t2.last; lexer = t2.lexer }

let change_lexer lexer t = { t with lexer = lexer }

let rec parse_internal lexer =
  let t = init lexer in
  parse_or t

and parse_or t =
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some (Token.Pipe) ->
    let empty = State.Empty { ptr = None } in
    let t1 = push empty t in
    let t2 = parse_internal lexer |> push empty in
    combine_split t1 t2
  | _ -> parse_mult t

and parse_mult t =
  (* TODO
  let atom = parse_atom t in
  let next, lexer = Lexer.next atom.lexer in
  match next with
  | _ -> atom
  *)
  parse_atom t

and parse_atom t =
  let next, lexer = Lexer.next t.lexer in
  match next with
  | Some (Token.Char ch) ->
    let atom = State.Atom (ch, { ptr = None }) in
    push atom t 
    |> change_lexer lexer
    |> parse_or
  | Some (Token.LeftBracket) ->
    let new_t = parse_internal lexer in
    let next, lexer = Lexer.next new_t.lexer in
    if next = Some Token.RightBracket then
      combine_linear t new_t 
      |> change_lexer lexer
      |> parse_or
    else
      raise (ParseError "expected `)` after `(`")
  | Some (Token.RightBracket) -> t
  | None -> change_lexer lexer t
  | _ -> parse_or t

let parse lexer =
  let t = parse_internal lexer |> push State.Match in
  (* can't be `None` because we pushed just pushed Match to it *)
  Option.get t.head
