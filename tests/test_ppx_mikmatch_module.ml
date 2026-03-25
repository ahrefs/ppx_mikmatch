open OUnit2

let%mikmatch username = {| alnum+ ('.' alnum+)* |}

type level = {%mikmatch| ("debug" | "info" | "warn" | "error" as level) |}

module M = struct
  let%mikmatch hex_lower = {| ['0'-'9' 'a'-'f'] |}

  module N = struct
    let%mikmatch hex = {| ['0'-'9' 'a'-'f' 'A'-'Z'] |}
    let something_else = "something else"
  end

  let something_else = "something else"
end

let test_basic_module_pattern _ =
  let match_hex = function%mikmatch {|/ M.hex_lower+ /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_hex "ab12");
  assert_equal "matched" (match_hex "deadbeef");
  assert_equal "no match" (match_hex "ABCD")

let test_nested_module_pattern _ =
  let match_hex = function%mikmatch {|/ M.N.hex+ /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_hex "aB12");
  assert_equal "matched" (match_hex "DeadBeef");
  assert_equal "no match" (match_hex "xyz")

let test_module_values _ =
  assert_equal "something else" M.something_else;
  assert_equal "something else" M.N.something_else

(* Test source order preservation *)

(* Test 1: Pattern definition before module that uses it *)
let%mikmatch numbers = {| ['0'-'9'] |}

module OrderTest1 = struct
  let%mikmatch with_numbers = {| numbers+ |}
end

let test_pattern_before_module _ =
  let match_numbers = function%mikmatch {|/ OrderTest1.with_numbers /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_numbers "123");
  assert_equal "matched" (match_numbers "999");
  assert_equal "no match" (match_numbers "abc")

(* Test 2: Pattern definition after module that defines a pattern *)
module OrderTest2 = struct
  let%mikmatch my_pattern = {| ['a'-'z']+ |}
end

let%mikmatch use_module_pattern = {| OrderTest2.my_pattern |}

let test_pattern_after_module _ =
  let match_pattern = function%mikmatch {|/ use_module_pattern /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_pattern "hello");
  assert_equal "matched" (match_pattern "world");
  assert_equal "no match" (match_pattern "123")

(* Test 3: Multiple modules with interleaved pattern definitions *)
let%mikmatch digit = {| ['0'-'9'] |}

module A = struct
  let%mikmatch word = {| digit+ |}
end

let%mikmatch a_word_alias = {| A.word |}

module B = struct
  let%mikmatch sentence = {| a_word_alias (' ' a_word_alias)* |}
end

let test_interleaved_modules _ =
  let match_sentence = function%mikmatch {|/ B.sentence /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_sentence "123 456");
  assert_equal "matched" (match_sentence "1 2 3");
  assert_equal "no match" (match_sentence "abc def")

(* Test 4: Deeply nested modules with cross-references *)
let%mikmatch base = {| ['a'-'z'] |}

module Outer = struct
  let%mikmatch outer_pat = {| base+ |}

  module Inner = struct
    let%mikmatch inner_pat = {| outer_pat '-' outer_pat |}

    module DeepInner = struct
      let%mikmatch deep_pat = {| inner_pat '_' inner_pat |}
    end
  end
end

let%mikmatch use_deep = {| Outer.Inner.DeepInner.deep_pat |}

let test_deeply_nested_modules _ =
  let match_deep = function%mikmatch {|/ use_deep /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_deep "abc-def_ghi-jkl");
  assert_equal "matched" (match_deep "a-b_c-d");
  assert_equal "no match" (match_deep "abc-def-ghi")

(* Test 5: Pattern using module pattern in the same module *)
module SelfRef = struct
  let%mikmatch pat1 = {| ['0'-'9']+ |}
  let%mikmatch pat2 = {| pat1 '.' pat1 |}
end

let test_self_referencing_module _ =
  let match_decimal = function%mikmatch {|/ SelfRef.pat2 /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_decimal "123.456");
  assert_equal "matched" (match_decimal "0.0");
  assert_equal "no match" (match_decimal "123")

(* Test 6: Multiple top-level patterns referencing different modules *)
module Mod1 = struct
  let%mikmatch pattern1 = {| ['a'-'c']+ |}
end

module Mod2 = struct
  let%mikmatch pattern2 = {| ['x'-'z']+ |}
end

let%mikmatch combined_from_modules = {| Mod1.pattern1 ' ' Mod2.pattern2 |}

let test_multiple_module_refs _ =
  let match_combined = function%mikmatch {|/ combined_from_modules /|} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_combined "abc xyz");
  assert_equal "matched" (match_combined "aaa zzz");
  assert_equal "no match" (match_combined "abc def")

let suite =
  "Module tests"
  >::: [
         "test_basic_module_pattern" >:: test_basic_module_pattern;
         "test_nested_module_pattern" >:: test_nested_module_pattern;
         "test_module_values" >:: test_module_values;
         "test_pattern_before_module" >:: test_pattern_before_module;
         "test_pattern_after_module" >:: test_pattern_after_module;
         "test_interleaved_modules" >:: test_interleaved_modules;
         "test_deeply_nested_modules" >:: test_deeply_nested_modules;
         "test_self_referencing_module" >:: test_self_referencing_module;
         "test_multiple_module_refs" >:: test_multiple_module_refs;
       ]

let () = run_test_tt_main suite
