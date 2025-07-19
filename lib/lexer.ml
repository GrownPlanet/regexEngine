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
    | '|' -> (Some Token.Or, advance t)
    | '?' -> (Some Token.Question, advance t)
    | '*' -> (Some Token.Star, advance t)
    | '+' -> (Some Token.Plus, advance t)
    | ch -> (Some (Token.Char ch), advance t)
