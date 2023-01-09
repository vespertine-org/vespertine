type expr =
  | Var of string
  | App of expr * expr
  | Fun of string * expr