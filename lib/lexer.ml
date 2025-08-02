type t = {
  input : string;
  idx : int
}

let init input = { input; idx = 0 }

let advance input = { input with idx = input.idx + 1 }

let next t =
  if t.idx >= String.length t.input then
    (None, t)
  else
    match t.input.[t.idx] with
    | '(' -> (Some Token.LeftBracket, advance t)
    | ')' -> (Some Token.RightBracket, advance t)
    | '|' -> (Some Token.Pipe, advance t)
    | '?' -> (Some Token.Question, advance t)
    | '*' -> (Some Token.Star, advance t)
    | '+' -> (Some Token.Plus, advance t)
    | '\\' ->
      let new_t = advance t in
      if new_t.idx >= String.length t.input then
        (None, new_t)
      else
        (Some (Token.Char new_t.input.[new_t.idx]), advance new_t)
    | ch -> (Some (Token.Char ch), advance t)
