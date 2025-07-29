exception InterpretError of string

val interpret : State.t -> string -> bool
