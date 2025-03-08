structure Parser = struct
  datatype Lexemes = Let | Eq | In | Arrow | Lambda| Id of string | Num of int

  val program = "let x = 5 in let y = \\z -> z in (y 5)"

  fun readWord ([], w) = (w, []) 
  | readWord ((x::xs), w) = if x = " " then (w, xs) else readWord(xs, x::w) 

  fun tokenize(#"l" :: #"e":: #"t" :: cs) = Let :: tokenize cs
    | tokenize(#"=" :: cs) = Eq :: tokenize cs
    | tokenize(#"-" :: #">" :: cs) = Arnizerow :: tokenize cs
    | tokenize(#"\\" :: cs) = Lambda :: tokenize cs
end

(* structure Haskell = struct
  ...
end *)
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
  | Plus of Haskell * Haskell
  | Minus of Haskell * Haskell
  | Times of Haskell * Haskell
  | Let of string * Haskell * Haskell
  | Lambda of string * Haskell
  | Fun of string * string list * Haskell * Haskell
  | Call of Haskell * Haskell
and Entry = VariableBinding of string * (Haskell * (Entry list));

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
                     extend_env(entry :: captured, rest))  (* Add if not present *)

fun search (var_string, env) =
    let
        fun showEnv [] = ""
          | showEnv (VariableBinding(var, _) :: xs) = "Var: " ^ var ^ " " ^ showEnv xs
    in
        print ("Searching for: " ^ var_string ^ " in env: " ^ showEnv env ^ "\n");
        case env of
          VariableBinding(variable, assignment)::enviroment =>
            if String.compare(var_string, variable) = EQUAL then 
              SOME (VariableBinding(variable, assignment))
            else 
              search(var_string, enviroment)
        | [] => NONE
    end


fun fromFunToLambda([], exp) = exp
  | fromFunToLambda ((x::xs), exp) = Lambda(x, fromFunToLambda(xs, exp))

fun  eval (HaskellType haskellType, _) = SOME haskellType
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
    val condition_eval = eval(condition, environment)
  in
    case condition_eval of
        SOME (Boolean true) => eval(conditionTrue, environment)  (* Only evaluate the true branch *)
      | SOME (Boolean false) => eval(conditionFalse, environment)  (* Only evaluate the false branch *)
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

  | eval (Let(var, exp, scope), env) =
    let
      (* val new_env =extend_env(env, [VariableBinding(var, (exp, env))]) *)
      val new_env = env @ [VariableBinding(var, (exp, env))]
    in
      eval(scope, new_env)
    end
  | eval(Lambda(arg, body), env) = SOME (Function(arg, body, env))
  | eval(Fun(name, vars, exp, exp'), env) =
    eval(Let(name, fromFunToLambda(vars, exp), exp'), env)
    (* let
        (* fun showEnv [] = ""
          | showEnv (VariableBinding(var, _) :: xs) = "Var: " ^ var ^ " " ^ showEnv xs *)
      val new_env = env @ [VariableBinding(var, (f, env))]
    in
    
      (* print ("Searching for: " ^ var ^ " in env: " ^ showEnv new_env ^ "\n"); *)
      eval(f, new_env)
    end *)
  | eval (Call(f, arg), env) =
  let
    (* Evaluate the function (f) eagerly, since the function itself doesn't need lazy evaluation) *)
    val f' = eval(f, env)
  in
    case f' of
        SOME (Function(x, body, close_env)) =>
            let
              (* val extended_env = extend_env(close_env, env) *)
              val extended_env = close_env @ env
              val arg_value = eval(arg, env)  (* Evaluate the argument lazily when needed *)
            in
              eval(body, VariableBinding(x, (HaskellType (unwrapOption arg_value), extended_env)) :: extended_env)
            end
      | _ => NONE
  end


(* val expr = Let(
  "t", 
  Lambda("x", Lambda("y", Lambda("z", Plus(Var("x"), Plus(Var("y"), Var("z")))))), (* t = \x -> \y -> \z -> x + y + z *)
  Call(Call(Call(Var("t"), ConstInt(5)), ConstInt(4)), ConstInt(1)) (* ((t 5) 4) 1 *)
)

(* val print = eval(expr, []) *) *)


(* val factorial_expr = Let(
  "factorial", 
  Lambda("n", If(Eq(Var "n", ConstInt 0), ConstInt 1, Times(Var "n" , Call(Var "factorial" , Minus(Var "n" , ConstInt 1 ))))), 
  Call(Var "factorial", ConstInt 7)   (* Apply factorial to 5 *)
)

val print = eval(factorial_expr, []) *)

(* val expr = Let("y", 
               Call(Lambda("x", Call(Var("x"), Var("x"))), 
                    Lambda("x", Call(Var("x"), Var("x")))), 
               ConstInt 5)

(* Now evaluate the expression *)
val result = eval(expr, [VariableBinding ("z", (HaskellType(Integer 5), []))]) *)

(* val expr = Let("y", 
               Call(Lambda("x", Call(Var("x"), Var("x"))), 
                    Lambda("x", Call(Var("x"), Var("x")))), 
               Var "y")

(* Now evaluate the expression *)
val result = eval(expr, [VariableBinding ("z", (HaskellType(Integer 5), []))]) *)

(* val scoop =Let("y", ConstInt 5, Let("y", Plus(Var "x", Var "x"), Let("x", ConstInt 10, Var "y")))
val print = eval(scoop, []) *)

(* val scoop =Let("z", ConstInt 5, Let("y", Lambda("a", Plus( Var"a", Var "z")), Let("z", ConstInt 10, Call(Var "y", ConstInt 10))))
val print = eval(scoop, []) *)

(* val scoop =Let("x", Lambda("a", Lambda("b", Plus(Var "a", Var "b"))), Call(Call(Var"x", ConstInt 5), ConstInt 6))
val print = eval(scoop, []) *)

(* val scoop =Let("z", ConstInt 5, Let("y", Lambda("a", Lambda("b", Plus( Var"a", Plus(Var "b", Var "z")))), Let("z", ConstInt 10, Call(Call(Var "y", ConstInt 10), ConstInt 1))))
val print = eval(scoop, []) *)

(* val test_fn = Fun("plus", ["x", "y"], Plus(Var "x", Var "y"), Call(Call( Var "plus", ConstInt 5), ConstInt 4))
val print = eval(test_fn, []) *)


val factorial = Fun("factorial", ["n"], If(Eq(Var "n", ConstInt 1), ConstInt 1, Times(Var "n", Call(Var "factorial", Minus(Var "n", ConstInt 1)))), Call(Var "factorial", ConstInt 5))
val print = eval(factorial, [])


