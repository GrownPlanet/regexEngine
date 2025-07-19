(* based on https://swtch.com/~rsc/regexp/regexp1.html and Thompson NFA *)

(* 
example:
  a(bb|cc)+a
expands to:
  state (Atom a)
  -> state Split (ref a) (
    state (Atom b) -> state (Atom b) -> state (Empty) (ref b),
    state (Atom c) -> state (Atom c) -> (ref b)
  )
  ref b -> state Split (
    (ref a),
    state (Atom a) -> state Match
  )
 *)

open RegexEngine

let rec lex lexer =
  match Lexer.next lexer with
  | (None, _) -> ()
  | (Some token, lexer) ->
      Token.pp token;
      lex lexer

let () =
  let input = "a(bb|cc)+a" in
  let lexer = Lexer.init input in
  lex lexer
