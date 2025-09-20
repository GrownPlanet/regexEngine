exception InterpretError of string

type indexed_state = {
  state : State.t;
  idx : int;
}

module StateOrdered : Set.OrderedType with type t = indexed_state = struct
  type t = indexed_state
  let compare = Stdlib.compare
end

module StateSet = Set.Make(StateOrdered)

let is_at_end t input = t.idx = String.length input

let rec match_char_class char tokens =
  match tokens with
  | [] -> false
  | token :: tail ->
    match token with
    | Token.Char match_char ->
      if char = match_char then
        true 
      else
        match_char_class char tail
    | _ ->
      raise (InterpretError "internal: got unexpected token in 'CharClass'")

let match_state input state =
  let is_in_range char first last =
    let num = int_of_char char in
    num >= (int_of_char first) && num <= (int_of_char last)
  in
  let match_atom atom char (next: State.t_ptr) =
    let new_state = [{ state = Option.get next.ptr; idx = state.idx + 1 }] in
    match atom with
    | State.Char match_char ->
      if char = match_char then new_state else []
    | State.WildCard -> new_state
    | State.Word ->
      if is_in_range char 'A' 'Z' || is_in_range char 'a' 'z' then
        new_state
      else
        []
    | State.Digit ->
      if is_in_range char '0' '9' then new_state else []
    | State.Whitespace ->
      (match char with
      | ' ' | '\n' | '\t' | '\r' -> new_state
      | _ -> [])
    | State.CharClass tokens ->
      if match_char_class char tokens then new_state else []
    | State.NotCharClass tokens ->
      if match_char_class char tokens then [] else new_state
  in
  match state.state with
  | State.Atom (atom, next) ->
    if state.idx < String.length input then
      let char = input.[state.idx] in
      match_atom atom char next
    else []
  | State.Split (next1, next2) ->
    [
      { state = Option.get next1.ptr; idx = state.idx };
      { state = Option.get next2.ptr; idx = state.idx }
    ]
  | State.Empty next ->
    [{ state = Option.get next.ptr; idx = state.idx }]
  | State.Match -> []

let interpret state input =
  let rec helper input memoization states =
    let next_states =
        (List.concat_map (match_state input) states)
        |> List.filter (fun state -> not (StateSet.mem state memoization))
    in
    match next_states with
    | [] -> false
    | _ ->
      let new_memoization =
        List.fold_left
          (fun acc state -> StateSet.add state acc)
          memoization
          next_states
      in
      let deduped_states = StateSet.to_list (StateSet.of_list next_states) in
      if List.exists
          (fun t -> t.state = State.Match && is_at_end t input)
          deduped_states
      then
        true
      else
        helper input new_memoization deduped_states
  in
  let idx_state = { state = state; idx = 0 } in
  let mem = StateSet.singleton idx_state in
  helper input mem [idx_state]
