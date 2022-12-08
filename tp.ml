type a_expression =
  | Variable of string
  | Constant of int
  | Addition of a_expression * a_expression
  | Opposite of a_expression
  | Equal of a_expression * a_expression ;;

type b_expression =
  | True
  | False
  | Variable of string
  | And of b_expression * b_expression
  | Opposite of b_expression
  | Inf of b_expression * b_expression
  | Equal of b_expression * b_expression
;;

type command =
  | Affectation of string * a_expression (*x:=a*)
  | Skip (* ne rien faire *)
  | Sequence of command * command (* c1 ; c2 *)
  | Condition  of b_expression * command * command (* if b then c1 else c2 *)
  | Loop of b_expression * command (* while b do c *)
;;

let rec com evalC= function 
  | Sequence (c1,c2) -> evalC c1 ; evalC c2 
  | Condition (b,c1,c2) -> if(b = True) then evalC c1 else evalC c2
  | Loop (b,c) -> while b=True do evalC c done
  | Affectation (x,a) -> ()
  | Skip -> ()
;;

let rec ca evalA = function 
  | Addition (a, b) -> evalA a + evalA b
  | Opposite (a) -> -1 * evalA a
  | Equal (e1,e2) -> if(evalA e1 = evalA e2) then 1 else 0 
;;

let rec cb evalB = function
  | True -> true
  | False -> false
  | And (False, _) | And (_, False) -> false
  | And (e1, e2) -> evalB e1 && evalB e2
  | Opposite (e) -> if(e = True) then true  else false
  | Inf (e1, e2) -> if(e1 < e2 )then true else false 
  | Equal ((e1 : b_expression) ,(e2 : b_expression)) -> if(evalB e1 = evalB e2)then true else false 
          (*| Equal ((e1 : a_expression) ,(e2 : a_expression)) ->  if(evalA e1 = evalA e2)then true else false*)
;;

let com1 =
  let z = True in 
  let x = 3 in 
  let y = 1 in
  Loop(z,Skip)(*Opposite(Equal(x,0)),y=x+y;x=x-1*)
;;
