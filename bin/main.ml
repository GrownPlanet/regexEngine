(* based on https://swtch.com/~rsc/regexp/regexp1.html and Thompson NFA *)

open RegexEngine

let match_regex pattern input =
  let lexer = Lexer.init pattern in
  let state = Parser.parse lexer in
  Interpreter.interpret state input

let test_match_regex pattern input expected =
  let result = (match_regex pattern input) in
  if result = expected then
    (print_endline "\027[32mtest passed!\027[0m";
    true)
  else
    (Printf.printf "\027[31mtest failed!\027[0m \"%s\" on \"%s\" got %s, but expected %s\n"
      pattern input
      (string_of_bool result)
      (string_of_bool expected);
    false)

let () =
  (* all thank chatgpt for writing these tests *)
  let test_cases = [
    ("a(b(c))", "abc", true);
    ("abc", "abcdefg", false);
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
    ("(this is a test)", "this is a test", true);
    ("()", "this is a test", false);
    ("(|)", "", true);
    ("(a|)", "a", true);
    ("(a|)", "", true);
    ("(|a)", "", true);
    ("(|a)", "a", true);
    ("a\\(bc", "a(bc", true);
    ("a?", "a", true);
    ("a?", "", true);
    ("ab?", "a", true);
    ("ab?", "ab", true);
    ("ab?c", "ac", true);
    ("ab?c", "abbc", false);
    ("a(bc)?d", "ad", true);
    ("a(b|c)?d", "acd", true);
    ("a*", "", true);
    ("a*", "a", true);
    ("a*", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa", true);
    ("a*b", "aab", true);
    ("ab*c", "abbbbbbbbbbbbc", true);
    ("a+", "", false);
    ("a+", "a", true);
    ("a+", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa", true);
    ("a+b", "aab", true);
    ("(ab)+c", "abbbbbbbbbbbbc", false);
    ("(ab)+c", "ababababc", true);
    (* the example from the article *)
    ("a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa", true);
  ] in
  let succeeded, failed = List.fold_left
    (fun (succeeded, failed) (pattern, input, expected) ->
      let result = test_match_regex pattern input expected in
      if result then (succeeded + 1, failed) else (succeeded, failed + 1)
    ) (0, 0) test_cases
  in
  Printf.printf "\n%d test cases succeeded and %d cases failed\n" succeeded failed
