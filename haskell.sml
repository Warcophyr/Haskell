datatype HaskellType = 
    Integer of int 
  | RealNumber of real
  | Character of char
  | Boolean of bool
  | Function of string * Haskell 

and Haskell = 
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
  | Times of Haskell * Haskell
  | Lamda of Haskell * Haskell;

datatype Entry = VariableBinding of string * (Haskell * (Entry list))| FunctionClosure of  string * (string * Haskell  * (Entry list));

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
      SOME (VariableBinding(_, (expression, associated_environment))) =>
        eval (expression, associated_environment)
    | _ => NONE)
  | eval (Eq(a, b), environment) =
    (case (eval(a, environment), eval(b, environment)) of
        (SOME (Integer a), SOME (Integer b)) =>
          SOME (Boolean (Int.compare(a, b) = EQUAL))
      | (SOME (RealNumber a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.compare(a, b) = EQUAL))
      | (SOME (Character a), SOME (Character b)) =>
          SOME(Boolean (Char.compare(a, b) = EQUAL))
      | (SOME (Boolean a), SOME (Boolean b)) =>
          SOME(Boolean (a = b))
      | _ => NONE) 
  | eval (Implies(a, b), environment) =
    (case (eval(a, environment), eval(b, environment)) of
      (SOME (Boolean a), SOME (Boolean b)) => SOME (Boolean (not a orelse b))
    | _ => NONE)
  | eval (If(condition, conditionTrue, conditionFalse), environment) =
    (case eval(condition, environment) of
      SOME (Boolean true) => eval (conditionTrue, environment)
    | SOME (Boolean false) => eval (conditionFalse, environment)
    |_ => NONE)
  | eval (Let(variable, expression, scope), environment) =
      eval(scope, VariableBinding(variable, (expression, environment)) :: environment)
  | eval (Fn (function, argument, body, scope), environment) =
      eval(scope, FunctionClosure (function, (argument, body, environment)):: environment)
  | eval (Call (function, expression), environment) =
    (case search(function, environment) of 
        SOME (FunctionClosure(_, (argument, body, associate_environment))) =>
          eval (body, FunctionClosure(function, (argument, body, associate_environment)) 
            :: VariableBinding(argument, (expression, environment))
            :: associate_environment
          )
      | _ => NONE )
  | eval (Plus(x, y), environment) =
    (case (eval(x, environment), eval(y, environment)) of
        (SOME (Integer x), SOME (Integer y)) => SOME (Integer(x + y))
      | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber(x + y))
      | _ => NONE)
  | eval (Minus(x, y), environment) =
    (case (eval(x, environment), eval(y, environment)) of
        (SOME (Integer x), SOME (Integer y)) => SOME (Integer(x - y))
      | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber(x - y))
      | _ => NONE)
  | eval (Times(x, y), environment) =
    (case (eval(x, environment), eval(y, environment)) of
        (SOME (Integer x), SOME (Integer y)) => SOME (Integer(x - y))
      | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber(x - y))
      | _ => NONE)


val x = Var("x")
(* val print = eval(x, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)

val y = Let("y", Plus(x,x), Var("y"))
(* val print = eval(y, []) *)
(* val print = eval(y, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)


val z = Fn("sum", "x", Plus(x, x), Call("sum", Var("x")))
(* val print = eval(z, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)


val t = Fn("omega", "x", Call("omega", HaskellType(Integer 5)), Call("omega", HaskellType(Integer 5)))
(* val print = eval(t, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)

val v = Var("v")
val print = eval(v, [FunctionClosure("v",("sum", Call("omega", Var("x")), []))])

(* val sum = 
  Fn (
    "sum", "x"
  ) *)

(* val a = Let("x", HaskellType(Integer 5), Times(Var "x", Times(HaskellType(Integer 5), HaskellType(Integer 4))))
val res = eval (a, []) *)

(* val factorial =
  Fn
    ( "factorial"
    , "n"
    , If
        ( Eq (Var "n", HaskellType (Integer 0))
        , HaskellType (Integer 1)
        , Times (Var "n", Call ("factorial", Minus
            (Var "n", HaskellType (Integer 1))))
        )
    , Call ("factorial", HaskellType (Integer 3))
    );

val resf = eval (factorial, []) *)

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
