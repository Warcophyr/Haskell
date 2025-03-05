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


fun unwrapOption (SOME x) = x
  | unwrapOption NONE = raise Fail "Empty option"

(* Helper function to check if a variable is already in the environment *)
fun is_in_env(var, []) = false
  | is_in_env(var, VariableBinding(v, _) :: rest) = (v = var) orelse is_in_env(var, rest)
  | is_in_env(var, FunctionClosure(f, _) :: rest) = (f = var) orelse is_in_env(var, rest);

(* Function to extend captured env with current env without overriding *)
fun extend_env(captured, current) =
    case current of
        [] => captured  (* No more variables to add *)
      | entry :: rest =>
          (case entry of
               VariableBinding(var, _) =>
                 if is_in_env(var, captured) then
                     extend_env(captured, rest)  (* Skip if already present *)
                 else
                     extend_env(entry :: captured, rest)  (* Add if not present *)
             | FunctionClosure(func, _) =>
                 if is_in_env(func, captured) then
                     extend_env(captured, rest)  (* Skip if already present *)
                 else
                     extend_env(entry :: captured, rest))  (* Add if not present *)

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
    end


(* A Thunk is a function that returns an evaluation *)
type 'a thunk = unit -> 'a

(* Helper function to create a thunk for lazy evaluation *)
fun delay_eval (expr,  environment )= fn () => eval(expr, environment)

and  eval (HaskellType haskellType, _) = SOME haskellType
  | eval (Var variable, environment) =
    (case search(variable, environment) of
      SOME (VariableBinding(_, (expression, associated_environment))) =>
        eval (expression, associated_environment)
    | _ => NONE)
  | eval (ConstInt n, _) = SOME (Integer n)
  | eval (ConstReal n, _) = SOME (RealNumber n)
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
  let
    val condition_thunk = delay_eval(condition, environment)
    val true_thunk = delay_eval(conditionTrue, environment)
    val false_thunk = delay_eval(conditionFalse, environment)
  in
    case condition_thunk () of
        SOME (Boolean true) => true_thunk ()  (* Only evaluate the true branch *)
      | SOME (Boolean false) => false_thunk ()  (* Only evaluate the false branch *)
      | _ => NONE
  end
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
    (* Create a thunk for the argument so it's evaluated lazily *)
    val arg_thunk = delay_eval(arg, env)
    (* Evaluate the function (f) eagerly, since the function itself doesn't need lazy evaluation) *)
    val f' = eval(f, env)
  in
    case f' of
        SOME (Function(x, body, close_env)) =>
            (* Now, instead of evaluating the argument immediately, pass the thunk *)
            let
              val extended_env = extend_env(close_env, env)  (* Extend the environment with the closure's environment *)
              val arg_value = arg_thunk ()  (* Evaluate the argument lazily when needed *)
            in
              eval(body, VariableBinding(x, (HaskellType (unwrapOption arg_value), extended_env)) :: extended_env)
            end
      | _ => NONE
  end
  (* | eval (Call(f, arg), env) =
    let
      val f' = eval(f, env)
      val arg' = eval(arg, env)
    in
      case (f', arg') of
            (SOME (Function(x, body, close_environment)), SOME value) =>
                (* Safely extend the captured environment with the current one *)
                let
                    val extended_env = extend_env(close_environment, env)
                in
                    eval(body, VariableBinding(x, (HaskellType value, extended_env)) :: extended_env)
                end
          | _ => NONE
    end *)
  | eval(Lambda(arg, body), env) = SOME (Function(arg, body, env))
  | eval (Let(var, exp, scope), env) =
  let
    (* Create thunks for exp and scope to delay their evaluation *)
    val exp_thunk = delay_eval(exp, env)
    (* val scope_thunk = delay_eval(scope, env) *)
  in
    case exp of
         Lambda(arg, body) =>
            let
              (* Create a placeholder for the recursive environment *)
              val placeholder = ref [] : Entry list ref
              val recEnv = VariableBinding(var, (HaskellType (Function(arg, body, !placeholder)), !placeholder)) :: env
              val () = placeholder := recEnv  (* Replace placeholder with actual recursive environment *)
            in
              eval(scope, recEnv)
            end
       | _ =>
            (case exp_thunk () of  (* Evaluate exp only when needed *)
                 SOME v => eval(scope, VariableBinding(var, (HaskellType v, env)) :: env)
               | NONE => NONE)
  end


   (* | eval (Let(var, exp, scope), env) =
    (case eval(exp, env) of
         SOME v =>
           (* For any expression (whether it’s a lambda or not), we bind
              the variable to the evaluated result.
              The binding stores a Haskell expression that when evaluated
              returns the value, ensuring the let is non-strict if needed. *)
           eval(scope, VariableBinding(var, (HaskellType v, env)) :: env)
       | NONE => NONE) *)


(* val expr = Lambda("y", Plus(Var "y", ConstInt 2))
(* val print = eval(expr, []) *)
val print = eval(Call(expr, ConstInt 1), [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)
(* val print = eval(expr, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)

(* val expif = Let("sum", If(Eq(Var"x", ConstInt 10), Var "x", Plus(Var "x", ConstInt 1)), Call(Var "sum", Var "x"))
val print = eval(expif, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)

(* val expl = Let("loop",
    Lambda("x",
        If(Eq(Var "x", ConstInt 10),
           Var "x",
           Call(Var "loop", Plus(Var "x", ConstInt 1)))),
    Call(Var "loop", Var "x"))
val print = eval(expl, [VariableBinding ("x", (HaskellType(Integer 5), []))]) *)


(* val expr = Let(
  "t", 
  Lambda("x", Lambda("y", Lambda("z", Plus(Var("x"), Plus(Var("y"), Var("z")))))), (* t = \x -> \y -> \z -> x + y + z *)
  Call(Call(Call(Var("t"), ConstInt(5)), ConstInt(4)), ConstInt(1)) (* ((t 5) 4) 1 *)
)

val print = eval(expr, []) *)


(* val factorial_expr = Let(
  "factorial", 
  Lambda("n", If(Eq(Var "n", ConstInt 0), ConstInt 1, Times(Var "n" , Call(Var "factorial" , Minus(Var "n" , ConstInt 1 ))))), 
  Call(Var "factorial", ConstInt 5)   (* Apply factorial to 5 *)
)

val print = eval(factorial_expr, []) *)

val expr = Let("y", 
               Call(Lambda("x", Call(Var("x"), Var("x"))), 
                    Lambda("x", Call(Var("x"), Var("x")))), 
               ConstInt 5)

(* Now evaluate the expression *)
val result = eval(expr, [VariableBinding ("z", (HaskellType(Integer 5), []))])
