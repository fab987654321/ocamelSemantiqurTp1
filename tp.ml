type a_expression =
  | Variable of string
  | Constant of int
  | Addition of a_expression * a_expression
  | Opposite of a_expression;;

type b_expression =
  | Variable of string
  | Constant of bool 
  | And of b_expression * b_expression
  | Opposite b_expression
  | Inf of b_expression * b_expression;;

type command =
  | Affectation of string * a_expression (*x:=a*)
  | Skip (* ne rien faire *)
  | Sequence of command * command (* c1 ; c2 *)
  | Condition  of b_expression * command * command (* if b then c1 else c2 *)
  | Loop of b_expression * command ;; (* while b do c *)
  
