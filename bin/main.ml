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

let () =
  (* let input = "a(bb|cc)+a" in *)
  let input = "abc(d|e)" in
  let lexer = Lexer.init input in
  let state = Parser.parse lexer in
  Interpreter.interpret state "abcd"
