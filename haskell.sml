datatype HaskellType = 
    Integer of int 
  | Real of real
  | Character of char
  | Boolean of bool 

datatype Haskell = 
    HaskellType of HaskellType
  | Var of string 
  | Eq of Haskell * Haskell
  | Implies of Haskell * Haskell
  | If of Haskell * Haskell * Haskell
  | Let of string * Haskell * Haskell
  | Fn of string * string * Haskell * Haskell
  | Call of string * Haskell
  | Plus of Haskell * Haskell
  | Minus of Haskell * Haskell
  | Times of Haskell * Haskell;

datatype Entry = VariableBinding of string * (Haskell * (Entry list))| FunctionClosure of  string * (string * Haskell * Haskell * (Entry list));

(* val env : Entry list [] *)

fun search (string, VariableBinding(variable, assignment):: enviroment) =
    if String.compare(string, variable) = EQUAL then 
      SOME (VariableBinding(variable, assignment))
    else 
      search(string, enviroment)
  | search(string, FunctionClosure(function, closere):: enviroment) =
    if String.compare(string, function) = EQUAL then 
      SOME (FunctionClosure(function, closere))
    else 
      search(string, enviroment)
  |search(_, []) = NONE;

fun eval (HaskellType haskellType, _) = SOME haskellType
  | eval (Var variable, environment) =
    (case search(variable, environment) of
      SOME (VariableBinding(_, (expression, associted_environment))) =>
        eval (expression, associated_enviroment)
    | _ => NONE)
  | eval (Implies(a, b), environment) =
    (case (eval(a, environment), eval(n, environment)) of
      (SOME (Boolean a), SOME (Boolean b)) => SOME (Boolean (not a orelse b))
    | _ => NONE)
  | eval (If(condition, condizionTrue, conditionFalse), environment) =
    (case eval(condition, environment) of
      SOME (Boolean true) => eval (condizionTrue, environment)
    | SOME (Boolean false) => eval (condizionFalse, environment)
    |_ => NONE)

(* datatype 'a env = Bind of string * 'a * 'a env | EmptyEnv;

datatype Value = IntVal of int | FunVal of (Value -> Value);

fun lookup str EmptyEnv = NONE
  | lookup str (Bind (var, VAL, rest)) =
      if str = var then SOME VAL
      else lookup str rest;

fun eval (Const n) env = IntVal n
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
  | eval (IfThenElse (cond, tExpr, fExpr)) env =
      (case eval cond env of
         IntVal n => if n <> 0 then eval tExpr env else eval fExpr env
       | _ => raise Fail "Condition must evaluate to an integer")
  | eval (Fn (x, body)) env = 
      FunVal (fn arg => eval body (Bind (x, arg, env)))
  | eval (Rec (fnExpr, argExpr)) env = 
      (case eval fnExpr env of
         FunVal f => f (eval argExpr env)
       | _ => raise Fail "Rec requires a function")
  | eval (Plus (p, q)) env = 
      (case (eval p env, eval q env) of
         (IntVal n1, IntVal n2) => IntVal (n1 + n2)
       | _ => raise Fail "Plus requires integer arguments")
  | eval (Minus (p, q)) env = 
      (case (eval p env, eval q env) of 
        (IntVal n1, IntVal n2) => IntVal (n1 - n2)
       | _ => raise Fail "Minus requires integer argument")
  | eval (Times (p, q)) env = 
      (case (eval p env, eval q env) of
        (IntVal n1, IntVal n2) => IntVal (n1 * n2)
       | _ => raise Fail "Times requires integer argument");

val env = Bind("n", IntVal 1, EmptyEnv);

val res = eval (Var "n") env;

val f = eval (Fn("n", Plus(Var "n", Const 1))) env *)
