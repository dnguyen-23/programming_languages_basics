(* This is a tree that represents the  *)

(* This is called a variant type *)
type expr = 
  | Num of int
  | Plus of expr * expr (*Remember that you could have 
                          (1 + 2 + (2 + 3)) which is not just 2 numbers
                          --This represents multiple expressions
                          *)
