type t = {
  input : string;
  idx : int
}

let init input = { input; idx = 0 }

let advance input = { input with idx = input.idx + 1 }

let parse_backslash t =
  if t.idx >= String.length t.input then
    (None, t)
  else
    match t.input.[t.idx] with
    | 'w' -> (Some Token.Word, advance t)
    | 'd' -> (Some Token.Digit, advance t)
    | 's' -> (Some Token.Whitespace, advance t)
    | ch -> (Some (Token.Char ch), advance t)

let next_char_class t =
  if t.idx >= String.length t.input then
    (None, t)
  else
    match t.input.[t.idx] with
    | '\\' -> advance t |> parse_backslash
    | ']' -> (Some Token.RightBracket, advance t)
    | ch -> (Some (Token.Char ch), advance t)

let next t =
  if t.idx >= String.length t.input then
    (None, t)
  else
    match t.input.[t.idx] with
    | '(' -> (Some Token.LeftParenthese, advance t)
    | ')' -> (Some Token.RightParenthese, advance t)
    | '[' -> (Some Token.LeftBracket, advance t)
    | ']' -> (Some Token.RightBracket, advance t)
    | '|' -> (Some Token.Pipe, advance t)
    | '?' -> (Some Token.Question, advance t)
    | '*' -> (Some Token.Star, advance t)
    | '+' -> (Some Token.Plus, advance t)
    | '\\' -> advance t |> parse_backslash
    | '.' -> (Some Token.WildCard, advance t)
    | ch -> (Some (Token.Char ch), advance t)
