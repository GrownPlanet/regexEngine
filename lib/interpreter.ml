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

let match_state input state =
  match state.state with
  | State.Atom (match_char, next) ->
    if state.idx < String.length input then
      let char = input.[state.idx] in
      if char = match_char then
        [{ state = Option.get next.ptr; idx = state.idx + 1 }]
      else []
    else []
  | State.Split (next1, next2) ->
    [
      { state = Option.get next1.ptr; idx = state.idx };
      { state = Option.get next2.ptr; idx = state.idx }
    ]
  | State.Empty (next) ->
    [{ state = Option.get next.ptr; idx = state.idx }]
  | State.Match -> [state]

let interpret state input =
  let rec helper states input =
    let next_states =
      StateSet.fold
        (fun state acc -> (match_state input state) @ acc)
        states
        []
    in
    match next_states with
    | [] -> false
    | _ ->
      let next_states_set = StateSet.of_list next_states in
      if StateSet.exists (fun t -> t.state = State.Match) next_states_set then
        true
      else
        helper next_states_set input
  in
  let set = StateSet.empty in
  let idx_state = {
    state = state;
    idx = 0;
  } in
  helper (StateSet.add idx_state set) input
