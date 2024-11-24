datatype Haskell = 
    Const of int
    | Var of string 
    | Let of string * Haskell * Haskell
    | IfThenElse of Haskell * Haskell * Haskell
    | Fn of string * Haskell
    | Rec of Haskell * Haskell
    | Plus of Haskell * Haskell
    (* | Times of Haskell * Haskell; *)

datatype 'a env = EmptyEnv | Bind of string * 'a * 'a env
(* datatype env = EmptyEnv | Bind of string * int *  env *)

fun lookup str EmptyEnv = NONE
  | lookup str (Bind (var, VAL, rest)) =
      if str = var then SOME VAL
      else lookup str rest;


fun myFn arg x body env =  body (Bind (x, arg, env)) 

fun eval (Const n) env = n
  | eval (Var str) env = 
      (case lookup str env of
        NONE => raise Fail ("Unbound variable: " ^ str)
      | SOME v => v)
  | eval (Let (x, p, q)) env = 
      let
          val v1 = eval p env
          val newEnv = Bind(x, v1, env)
      in 
        eval q newEnv 
      end
  | eval (IfThenElse (cond , tExpr, fExpr)) env =
    (if eval cond env <> 0 then eval tExpr env
    else eval fExpr env)
  | eval (Fn (x, body)) env = (fn arg => eval body (Bind(x, arg, env)))
  | eval (Rec (fnExpr, argExpr)) env = 
    let 
      val fnVal = eval fnExpr env 
      val argVal = eval argExpr env 
    in
      fnVal argVal 
    end
  | eval (Plus (p, q)) env = (eval p env) + (eval q env);

(* val exp = Let ("x", Const 5, Plus(Var "x", Const 3));
val res = eval exp EmptyEnv; *)


(* val expr =eval(Const 1) EmptyEnv;
val env = Bind("x", 5, EmptyEnv);
val env = eval (Var "x") env; *)
(* val env = Bind("x", 5, EmptyEnv);
val expr = Var "x"
val result = eval expr env *)
(* eval (Plus((Const 1), (Const 2))) *)
