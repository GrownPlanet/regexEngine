type t

val init : string -> t
val next : t -> Token.t option * t
val next_char_class : t -> Token.t option * t
