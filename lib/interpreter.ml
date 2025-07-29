exception InterpretError of string

let interpret state input =
  let _ = state in
  let _ = input in
  raise (InterpretError "not implemented yet")
