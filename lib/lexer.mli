type t

val init : string -> t
val next : t -> Token.t option * t
