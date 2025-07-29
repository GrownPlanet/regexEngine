(* based on https://swtch.com/~rsc/regexp/regexp1.html and Thompson NFA *)

open RegexEngine

let match_regex pattern input = 
  let lexer = Lexer.init pattern in
  let state = Parser.parse lexer in
  Interpreter.interpret state input

let test_match_regex pattern input expected =
  let result = (match_regex pattern input) in
  if result = expected then
    print_endline "test passed!"
  else
    Printf.printf "test failed! \"%s\" on \"%s\" got %s but expected %s\n"
      pattern input
      (string_of_bool result)
      (string_of_bool expected)
;;

let () =
  (* all thank chatgpt for writing these tests *)
  let test_cases = [
    ("a|b", "b", true);
    ("a|b", "c", false);
    ("(x|y)", "x", true);
    ("(x|y)", "z", false);
    ("(a|b|c)", "c", true);
    ("(a|b|c)", "d", false);
    ("(ab|(cd|ef))", "ef", true);
    ("(ab|(cd|ef))", "gh", false);
    ("h(i|o)p", "hip", true);
    ("h(i|o)p", "hup", false);
    ("(1|2)(3|4)", "24", true);
    ("(1|2)(3|4)", "12", false);
    ("(yes|no)", "yes", true);
    ("(yes|no)", "maybe", false);
  ] in
  List.iter (fun (pattern, input, expected) ->
    test_match_regex pattern input expected
  ) test_cases
