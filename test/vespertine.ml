open Core

let%expect_test "addition" =
  printf "%d" (1 + 2);
  [%expect {| 3 |}]

(* let%expect_test "lambda" =
   parse "do: |s| s";
   [%expect {|Interp.Ast.expr = Interp.Ast.Fun ("x", Interp.Ast.Var "x")|}] *)
