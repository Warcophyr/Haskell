datatype Haskell = 
    Const of int 
    | Var of string 
    (* | Let of string * Haskell * Haskell *)
    | Plus of Haskell * Haskell
    (* | Condiscion of Haskell * Haskell; *)
    (* | If_Then_Else of Condiscion * Haskell * Haskell
    | Fn of Var * Haskell; *)

datatype env = EmptyEnv | Bind of string * int * env

fun lookup str EmptyEnv = NONE
  | lookup str (Bind (var, VAL, rest)) =
      if str = var then SOME VAL
      else lookup str rest;

fun eval (Const n) env = n
  | eval (Plus (a, b)) env = (eval a env) + (eval b env)
  | eval (Var str) env = 
      case lookup str env of
        NONE => raise Fail ("Unbound variable: " ^ str)
      | SOME v => v;

val expr =eval(Const 1) EmptyEnv;
(* val env = Bind("x", 5, EmptyEnv);
val expr = Var "x"
val result = eval expr env *)
(* eval (Plus((Const 1), (Const 2))) *)
