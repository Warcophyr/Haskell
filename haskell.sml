datatype HaskellType = 
    Integer of int 
  | RealNumber of real
  | Character of char
  | Boolean of bool
  | Function of string * Haskell * Entry list 

and Haskell = 
    HaskellType of HaskellType
  | ConstInt of int
  | ConstReal of real
  | Var of string
  | Eq of Haskell * Haskell
  | Implies of Haskell * Haskell
  | If of Haskell * Haskell * Haskell
  | Let of string * Haskell * Haskell
  (* | Fn of string * string * Haskell * Haskell *)
  | Call of Haskell * Haskell
  | Lambda of string * Haskell
  | Plus of Haskell * Haskell
  | Minus of Haskell * Haskell
  | Times of Haskell * Haskell
  | Delay of (unit -> Haskell)
and Entry = VariableBinding of string * (Haskell * (Entry list))| FunctionClosure of  string * (string * Haskell  * (Entry list));

(* val env : Entry list [] *)
fun toString (SOME (Integer n)) = "Integer " ^ Int.toString n
  | toString (SOME(RealNumber n)) = "RealNumber " ^ Real.toString n
  | toString (SOME(Character c)) = "Character " ^ Char.toString c
  | toString (SOME(Boolean b)) = "Boolean " ^ Bool.toString b
  | toString (SOME(Function(_, _, _))) = "Function"
  | toString NONE = "none"

fun toString_ (Integer n) = "Integer " ^ Int.toString n
  | toString_ (RealNumber n) = "RealNumber " ^ Real.toString n
  | toString_ (Character c) = "Character " ^ Char.toString c
  | toString_ (Boolean b) = "Boolean " ^ Bool.toString b
  | toString_ (Function(_, _, _)) = "Function"

fun search (string, env) =
    let
        fun showEnv [] = ""
          | showEnv (VariableBinding(var, _) :: xs) = "Var: " ^ var ^ " " ^ showEnv xs
          | showEnv (FunctionClosure(func, _) :: xs) = "Func: " ^ func ^ " " ^ showEnv xs
    in
        print ("Searching for: " ^ string ^ " in env: " ^ showEnv env ^ "\n");
        case env of
          VariableBinding(variable, assignment)::enviroment =>
            if String.compare(string, variable) = EQUAL then 
              SOME (VariableBinding(variable, assignment))
            else 
              search(string, enviroment)
        | FunctionClosure(function, closere)::enviroment =>
            if String.compare(string, function) = EQUAL then 
              SOME (FunctionClosure(function, closere))
            else 
              search(string, enviroment)
        | [] => NONE
    end;

fun eval (Delay f,environment) = 
    let val result = f()
    in 
      eval(result, environment)
    end
  | eval (HaskellType haskellType, _) = SOME haskellType
  | eval (Var variable, environment) =
    (case search(variable, environment) of
      SOME (VariableBinding(_, (expression, associated_environment))) =>
        eval (expression, associated_environment)
    | _ => NONE)
  | eval (ConstInt(n), environment) = SOME (Integer n)
  | eval (ConstReal(n), environment) = SOME (RealNumber n)
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
  (* | eval (Let(variable, expression, scope), environment) =
    eval(scope, VariableBinding(variable, (expression, environment)) :: environment) *)
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
        (SOME (Integer x), SOME (Integer y)) => SOME (Integer(x * y))
      | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber(x * y))
      | _ => NONE)
  | eval (Call(f, arg), env) =
    let
      val f' = eval(f, env)
      val arg' = eval(arg, env)
    in
      case (f', arg') of
            (SOME (Function(x, body, close_environment)), SOME value) =>
                (* Call the function with its own closure in the environment *)
                eval(body, VariableBinding(x, (HaskellType value, close_environment)) :: close_environment)
          | _ => NONE
    end
  | eval(Lambda(arg, body), env) = SOME (Function(arg, body, env))
  | eval(Let(var, exp, scope), env) =
    let
      val exp' = eval(exp, env)
    in
      case SOME exp of
       SOME (Lambda(arg, body)) =>
                (* Here we close over the environment by adding the function definition to it *)
                eval(scope, VariableBinding(var, (Lambda(arg, body), env)) :: env)
            | _ => NONE
    end


(* val expr = Lambda("y", Plus(Var "x", ConstInt 2))
val print = eval(Call(expr, Var"x"), [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)
(* val print = eval(expr, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)

val expl = Let("y", Call(Var "x", Lambda("x", Plus(Var"z", ConstInt 1))), Call(Var "x", ConstInt 1))
val print = eval(expl, [VariableBinding ("z", (HaskellType(Integer 5), []))])

