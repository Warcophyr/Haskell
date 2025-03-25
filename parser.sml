structure Lexemes = struct
    datatype Lexemes = 
        Let 
        | Eq                  (* Token for '=' *)
        | And                 (* Token for '&&' *)
        | Or                  (* Token for '||' *)
        | In                  (* Token for 'in' *)
        | Arrow               (* Token for '->' *)
        | Lambda              (* Token for '\' *)
        | Add                 (* Token for '+' *)
        | Sub                 (* Token for '-' *)
        | Mul                 (* Token for '*' *)
        | Div                 (* Token for '/' *)
        | Mod                 (* Token for '%' *)
        | Id of string        (* Token for identifiers *)
        | Integer of int      (* Token for integers *)
        | RealNumber of real  (* Token for real numbers *)
        | LParen              (* Token for '(' *)
        | RParen              (* Token for ')' *)
        | If                  (* Token for 'if' *)
        | Then                (* Token for 'then' *)
        | Else                (* Token for 'else' *)
        | Comp                (* Token for '==' *)
        | Gt                  (* Token for '>' *)
        | Ge                  (* Token for '>=' *)
        | Lt                  (* Token for '<' *)
        | Le                  (* Token for '<=' *)
        | Not                 (* Token for 'not' *)

    (* Helper function to read a word or identifier *)
    fun readWord ([], w) = (implode (rev w), [])
      | readWord (c :: xs, w) =
          if Char.isAlpha c then
            readWord (xs, c :: w)
          else
            (implode (rev w), c :: xs)

    (* Helper function to read numbers (both integers and reals) *)
    fun readNumber ([], n, hasDot) = 
      let val numStr = implode (rev n)
      in if hasDot 
         then (RealNumber (valOf (Real.fromString numStr)), [])
         else (Integer (valOf (Int.fromString numStr)), [])
      end
  | readNumber (#" " :: xs, n, hasDot) = 
      let val numStr = implode (rev n)
      in if hasDot 
         then (RealNumber (valOf (Real.fromString numStr)), xs)
         else (Integer (valOf (Int.fromString numStr)), xs)
      end
  | readNumber (#"." :: xs, n, false) = readNumber (xs, #"." :: n, true)
  | readNumber (#"." :: _, _, true) = raise Fail "Invalid number format: multiple dots"
  | readNumber (c :: xs, n, hasDot) =
      if Char.isDigit c then
          readNumber (xs, c :: n, hasDot)
      else
          let val numStr = implode (rev n)
          in if hasDot 
             then (RealNumber (valOf (Real.fromString numStr)), c :: xs)
             else (Integer (valOf (Int.fromString numStr)), c :: xs)
          end

    (* Main function to tokenize the input *)
    fun tokenize [] = []
    | tokenize (#" " :: cs) = tokenize cs  (* Skip spaces *)
    | tokenize (#"l" :: #"e" :: #"t" :: cs) = Let :: tokenize cs
    | tokenize (#"i" :: #"f" :: cs) = If :: tokenize cs 
    | tokenize (#"t" :: #"h" :: #"e" :: #"n" :: cs) = Then :: tokenize cs
    | tokenize (#"e" :: #"l" :: #"s" :: #"e" :: cs) = Else :: tokenize cs 
    | tokenize (#"n" :: #"o" :: #"t" :: cs) = Not :: tokenize cs
    | tokenize (#"=" :: #"=" :: cs) = Comp :: tokenize cs
    | tokenize (#">" :: #"=" :: cs) = Ge :: tokenize cs
    | tokenize (#">" ::  cs) = Gt :: tokenize cs
    | tokenize (#"<" :: #"=" :: cs) = Le :: tokenize cs
    | tokenize (#"<" :: cs) = Lt :: tokenize cs
    | tokenize (#"i" :: #"n" :: cs) = In :: tokenize cs
    | tokenize (#"+" :: cs) = Add :: tokenize cs   (* Token for '+' *)
    | tokenize (#"-" :: #">" :: cs) = Arrow :: tokenize cs
    | tokenize (#"-" :: cs) = Sub :: tokenize cs  (* Token for '-' *)
    | tokenize (#"*" :: cs) = Mul :: tokenize cs  (* Token for '*' *)
    | tokenize (#"/" :: cs) = Div :: tokenize cs    (* Token for '/' *)
    | tokenize (#"%" :: cs) = Mod :: tokenize cs    (* Token for '/' *)
    | tokenize (#"\\" :: cs) = Lambda :: tokenize cs
    | tokenize (#")" :: []) = RParen :: []
    | tokenize (#"(" :: cs) = LParen :: tokenize cs
    | tokenize (#")" :: cs) = RParen :: tokenize cs
    | tokenize (#"&" :: #"&" :: cs) = And :: tokenize cs 
    | tokenize (#"|" :: #"|" :: cs) = Or :: tokenize cs
    | tokenize (#"=" :: cs) =  Eq :: tokenize cs 
    | tokenize (c :: cs) = 
        if Char.isAlpha c then
            let val (word, rest) = readWord (c :: cs, [])
            in Id word :: tokenize rest end
        else if Char.isDigit c orelse c = #"." then
            let val (num, rest) = readNumber (c :: cs, [], false)
            in num :: tokenize rest end
        else tokenize cs  (* Ignore unrecognized characters *)

    (* Function to run the tokenizer on the program *)
    
end


structure Parser = struct
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
    | Comp of Haskell * Haskell
    | Not of Haskell
    | Gt of Haskell * Haskell 
    | Ge of Haskell * Haskell 
    | Lt of Haskell * Haskell 
    | Le of Haskell * Haskell
    | Implies of Haskell * Haskell
    | And of Haskell * Haskell
    | Or of Haskell * Haskell
    | If of Haskell * Haskell * Haskell
    | Add of Haskell * Haskell
    | Sub of Haskell * Haskell
    | Mul of Haskell * Haskell
    | Div of Haskell * Haskell
    | Mod of Haskell * Haskell
    | Let of string * Haskell * Haskell
    | Lambda of string * Haskell
    | Fun of string * string list * Haskell * Haskell
    | Call of Haskell * Haskell
    and Entry = VariableBinding of string * (Haskell * (Entry list));


    (* exception ParseError of string *)

    fun expect expected (t::ts) =
    (case (expected, t) of
         (Lexemes.RParen, Lexemes.RParen) => ts
       | (Lexemes.LParen, Lexemes.LParen) => ts
       | (Lexemes.Let, Lexemes.Let)       => ts
       | (Lexemes.In, Lexemes.In)         => ts
       | (Lexemes.Eq, Lexemes.Eq)         => ts
       | (Lexemes.Arrow, Lexemes.Arrow)   => ts
       | (Lexemes.And, Lexemes.And)         => ts
       | (Lexemes.Or, Lexemes.Or)         => ts
       | (Lexemes.Lambda, Lexemes.Lambda) => ts
       | (Lexemes.Add, Lexemes.Add)     => ts
       | (Lexemes.Sub, Lexemes.Sub)   => ts
       | (Lexemes.Mul, Lexemes.Mul)   => ts
       | (Lexemes.Div, Lexemes.Div)   => ts
       | (Lexemes.Comp, Lexemes.Comp)     => ts 
       | (Lexemes.If, Lexemes.If)         => ts 
       | (Lexemes.Then, Lexemes.Then)     => ts 
       | (Lexemes.Else, Lexemes.Else)     => ts 
       | _ => raise Fail "Unexpected token")
  | expect _ [] = raise Fail "Unexpected end of input"


    fun parser ts =
    let
        (* Primary expressions: variables, numbers, and parenthesized expressions *)
        fun parserPrimary (Lexemes.Id name :: ts) = (Var name, ts)
          | parserPrimary (Lexemes.Integer n :: ts) = (ConstInt n, ts)
          | parserPrimary (Lexemes.RealNumber n :: ts) = (ConstReal n, ts)
          | parserPrimary (Lexemes.LParen :: ts) =
                let
                    val (exprs, ts') = parseExprList ts
                    val expr = 
                        case exprs of
                            [] => raise Fail "Expected expression inside parentheses"
                          | e :: es => List.foldl (fn (acc, e) => Call(e, acc)) e es
                in
                    (expr, expect Lexemes.RParen ts')
                end
          | parserPrimary (Lexemes.Lambda :: Lexemes.Id name :: Lexemes.Arrow :: ts) =
            let 
                val (body, ts') = parser ts
            in 
                (Lambda(name, body), ts')
            end 
          | parserPrimary _ = raise Fail "Expected primary expression"

        (* Parse a list of expressions until a right parenthesis is reached *)
        and parseExprList ts =
            let
                fun aux (acc, Lexemes.RParen :: ts) = (List.rev acc, Lexemes.RParen :: ts)
                  | aux (acc, ts) =
                        let
                            val (expr, ts') = parser ts
                        in
                            aux (expr :: acc, ts')
                        end
            in
                aux ([], ts)
            end

        (* Arithmetic expressions with left-associative operators *)
        and parserArithmetic ts =
            let
                val (lhs, ts') = parserCall ts
                fun aux (lhs, ts) =
                    case ts of
                      Lexemes.Add :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Add(lhs, rhs), ts'')
                        end
                    | Lexemes.Sub :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Sub(lhs, rhs), ts'')
                        end
                    | Lexemes.Mul :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Mul(lhs, rhs), ts'')
                        end
                    | Lexemes.Div :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Div(lhs, rhs), ts'')
                        end
                    | Lexemes.Mod :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Mod(lhs, rhs), ts'')
                        end
                    | _ => (lhs, ts)
            in
                aux(lhs, ts')
            end

        (* Lambda expressions *)
        and parserLambda (Lexemes.Lambda :: Lexemes.Id name :: Lexemes.Arrow :: ts) =
            let
                (* Check if the body is another lambda *)
                val (body, ts') =
                    case ts of
                        (Lexemes.Lambda :: _) => parserLambda ts  (* Recursive call for nested lambdas *)
                      | _ => parser ts  (* Use parserArithmetic for non-lambda bodies *)
            in
                (Lambda(name, body), ts')
            end
          | parserLambda ts = parser ts

        (* Function call parsing *)
        and parserCall ts =
            let
                val (lhs, ts') = parserPrimary ts
                fun aux (lhs, Lexemes.Id _ :: _) =
                    let
                        val (rhs, ts'') = parserPrimary ts'
                    in
                        aux(Call(lhs, rhs), ts'')
                    end
                  | aux (lhs, Lexemes.Integer _ :: _) =
                    let
                        val (rhs, ts'') = parserPrimary ts'
                    in
                        aux(Call(lhs, rhs), ts'')
                    end
                  | aux (lhs, Lexemes.RealNumber _ :: _) =
                    let
                        val (rhs, ts'') = parserPrimary ts'
                    in
                        aux(Call(lhs, rhs), ts'')
                    end
                  | aux res = res
            in
                aux (lhs, ts')
            end

        and parserCond ts =
            let
              val (lhs, ts') = parserComp ts
              fun aux (lhs, ts) =
                    case ts of
                      Lexemes.And :: ts' =>
                        let
                          val (rhs, ts'') = parserComp ts'
                        in
                          aux(And(lhs, rhs), ts'')
                        end
                    | Lexemes.Or :: ts' =>
                        let
                          val (rhs, ts'') = parserComp ts'
                        in
                          aux(Or(lhs, rhs), ts'')
                        end
                    | _ => (lhs, ts)
            in
              aux(lhs, ts')
            end
          
        and parserComp ts =
            let
              val (lhs, ts') = parserArithmetic ts
              fun aux (lhs, ts) =
                    case ts of
                      Lexemes.Comp :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Comp(lhs, rhs), ts'')
                        end
                    | Lexemes.Ge :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Ge(lhs, rhs), ts'')
                        end
                    | Lexemes.Gt :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Gt(lhs, rhs), ts'')
                        end
                    | Lexemes.Le :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Le(lhs, rhs), ts'')
                        end
                    | Lexemes.Lt :: ts' =>
                        let
                          val (rhs, ts'') = parserCall ts'
                        in
                          aux(Lt(lhs, rhs), ts'')
                        end
                    | _ => (lhs, ts)
            in
              aux(lhs, ts')
            end
        and parserIf (Lexemes.If :: ts) =
            let
                val (condition, ts1) = parserCond ts
                val ts2 = expect Lexemes.Then ts1
                val (trueBranch, ts3) = parser ts2
                val ts4 = expect Lexemes.Else ts3
                val (falseBranch, ts5) = parser ts4
            in
                (If(condition, trueBranch, falseBranch), ts5)
            end
            | parserIf ts = parserPrimary ts
        and parserLet (Lexemes.Let :: Lexemes.Id name :: Lexemes.Eq :: ts) =
            let
                val (exp, ts1) =
                  case ts of 
                      Lexemes.Lambda :: _ => parserLambda ts
                    | Lexemes.If :: _ => parserIf ts
                    | Lexemes.Let :: _ => parserLet ts 
                    | _ => parserArithmetic ts

                (* Parse the bound expression *)
                (* Expect an 'in' token *)
                val ts2 =
                    case ts1 of
                        Lexemes.In :: rest => rest
                      | _ => raise Fail "Expected 'in' after let binding"
                (* Parse the body expression *)
                val (body, ts3) = parser ts2
            in
                (Let(name, exp, body), ts3)
            end
          | parserLet ts = parserLambda ts

        and parserFun ts =
          case ts of
              Lexemes.Id f :: rest =>
                let
                  (* Helper to accumulate parameters until we hit an Eq token.
                    We use tail recursion with an accumulator (in reverse order)
                    and reverse it at the end. *)
                  fun parseParams (acc, ts) =
                        case ts of
                            Lexemes.Eq :: ts' => (List.rev acc, ts')
                          | Lexemes.Id s :: ts' => parseParams (s :: acc, ts')
                          | _ => raise Fail "Expected '=' in function definition"
                  val (params, afterEq) = parseParams ([], rest)
                  (* Parse the function body expression *)
                  val (bodyExpr, tokensAfterBody) = parser afterEq
                  (* Check if there is an 'in' token for a continuation expression.
                    If not, default the continuation to the function itself (Var f) *)
                  val (contExpr, remainingTokens) =
                      case tokensAfterBody of
                          Lexemes.In :: ts' =>
                            let
                              val (ce, ts'') = parser ts'
                            in
                              (ce, ts'')
                            end
                        | _ => (Var f, tokensAfterBody)
                in
                  (Fun(f, params, bodyExpr, contExpr), remainingTokens)
                end
            | _ => raise Fail "Function definition must start with an identifier"

        (* Expect a specific token; if not found, raise an error *)
    in
        case ts of
        Lexemes.Let :: _ => parserLet ts
       | Lexemes.If  :: _ => parserIf ts
       | Lexemes.Id t1 :: Lexemes.Id t2 :: Lexemes.RParen :: _  => parserArithmetic ts
       | Lexemes.Id t1 :: Lexemes.Id t2 :: _  => parserFun ts
       | _             => parserArithmetic ts
    end


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
        (* print ("Searching for: " ^ var_string ^ " in env: " ^ showEnv env ^ "\n"); *)
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
  | eval (Var var, env) =
    (case search(var, env) of
      SOME (VariableBinding(_, (exp, ass_env))) =>
        eval (exp, ass_env)
    | _ => NONE)
  | eval (ConstInt n, _) = SOME (Integer n)
  | eval (ConstReal n, _) = SOME (RealNumber n)
  | eval (Comp(a, b), env) =
    (case (eval(a, env), eval(b, env)) of
        (SOME (Integer a), SOME (Integer b)) =>
          SOME (Boolean (Int.compare(a, b) = EQUAL))
      | (SOME (RealNumber a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.compare(a, b) = EQUAL))
      | (SOME (Integer a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.compare( Real.fromInt a, b) = EQUAL))
      | (SOME (RealNumber a), SOME (Integer b)) =>
          SOME (Boolean(Real.compare(a, Real.fromInt b) = EQUAL))
      | (SOME (Character a), SOME (Character b)) =>
          SOME(Boolean (Char.compare(a, b) = EQUAL))
      | (SOME (Boolean a), SOME (Boolean b)) =>
          SOME(Boolean (a = b))
      | _ => NONE)
  | eval(Not a, env) = 
    (case eval(a, env) of 
        (SOME (Boolean a)) => SOME (Boolean (not a))
        | _ => NONE)
  | eval (Gt(a, b), env) =
    (case (eval(a, env), eval(b, env)) of
        (SOME (Integer a), SOME (Integer b)) =>
          SOME (Boolean (Int.>(a, b)))
      | (SOME (RealNumber a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.>(a, b)))
      | (SOME (Integer a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.>( Real.fromInt a, b)))
      | (SOME (RealNumber a), SOME (Integer b)) =>
          SOME (Boolean(Real.>(a, Real.fromInt b)))
      | (SOME (Character a), SOME (Character b)) =>
          SOME(Boolean (Char.>(a, b)))
      | _ => NONE)
  | eval (Ge(a, b), env) =
    (case (eval(a, env), eval(b, env)) of
        (SOME (Integer a), SOME (Integer b)) =>
          SOME (Boolean (Int.>=(a, b)))
      | (SOME (RealNumber a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.>=(a, b)))
      | (SOME (Integer a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.>=( Real.fromInt a, b)))
      | (SOME (RealNumber a), SOME (Integer b)) =>
          SOME (Boolean(Real.>=(a, Real.fromInt b)))
      | (SOME (Character a), SOME (Character b)) =>
          SOME(Boolean (Char.>=(a, b)))
      | _ => NONE)
  | eval (Lt(a, b), env) =
    (case (eval(a, env), eval(b, env)) of
        (SOME (Integer a), SOME (Integer b)) =>
          SOME (Boolean (Int.<(a, b)))
      | (SOME (RealNumber a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.<(a, b)))
      | (SOME (Integer a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.<( Real.fromInt a, b)))
      | (SOME (RealNumber a), SOME (Integer b)) =>
          SOME (Boolean(Real.<(a, Real.fromInt b)))
      | (SOME (Character a), SOME (Character b)) =>
          SOME(Boolean (Char.<(a, b)))
      | _ => NONE)
  | eval (Le(a, b), env) =
    (case (eval(a, env), eval(b, env)) of
        (SOME (Integer a), SOME (Integer b)) =>
          SOME (Boolean (Int.<=(a, b)))
      | (SOME (RealNumber a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.<=(a, b)))
      | (SOME (Integer a), SOME (RealNumber b)) =>
          SOME (Boolean(Real.<=( Real.fromInt a, b)))
      | (SOME (RealNumber a), SOME (Integer b)) =>
          SOME (Boolean(Real.<=(a, Real.fromInt b)))
      | (SOME (Character a), SOME (Character b)) =>
          SOME(Boolean (Char.<=(a, b)))
      | _ => NONE)
  | eval (And(a, b), env) =
      let
        val a' = eval(a, env)
      in
        case a' of 
           SOME (Boolean false) => SOME (Boolean false)
          |SOME (Boolean true) => 
            let
              val b' = eval(b, env)
            in
              case b' of 
                  SOME (Boolean false) => SOME (Boolean false)
                | SOME (Boolean true) => SOME (Boolean true)
                | _ => NONE
            end
          | _ => NONE
      end
  | eval(Or(a, b), env) =
      let
        val a' = eval(a, env)
      in
        case a' of 
           SOME (Boolean true) => SOME (Boolean true)
          |SOME (Boolean false) => 
            let
              val b' = eval(b, env)
            in
              case b' of 
                  SOME (Boolean false) => SOME (Boolean false)
                | SOME (Boolean true) => SOME (Boolean true)
                | _ => NONE
            end
          | _ => NONE
      end
  | eval (Implies(a, b), env) =
    (case (eval(a, env), eval(b, env)) of
      (SOME (Boolean a), SOME (Boolean b)) => SOME (Boolean (not a orelse b))
    | _ => NONE)
  | eval (If(condition, conditionTrue, conditionFalse), env) =
      let
        val condition_eval = eval(condition, env)
      in
        case condition_eval of
            SOME (Boolean true) => eval(conditionTrue, env)  (* Only evaluate the true branch *)
          | SOME (Boolean false) => eval(conditionFalse, env)  (* Only evaluate the false branch *)
          | _ => NONE
      end
  | eval (Add(x, y), env) =
  (case (eval(x, env), eval(y, env)) of
      (SOME (Integer x), SOME (Integer y)) => SOME (Integer (x + y))
    | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber (x + y))
    | (SOME (Integer x), SOME (RealNumber y)) => SOME (RealNumber (Real.fromInt x + y))  
    | (SOME (RealNumber x), SOME (Integer y)) => SOME (RealNumber (x + Real.fromInt y))  
    | _ => NONE)
  | eval (Sub(x, y), env) =
    (case (eval(x, env), eval(y, env)) of
      (SOME (Integer x), SOME (Integer y)) => SOME (Integer (x - y))
    | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber (x - y))
    | (SOME (Integer x), SOME (RealNumber y)) => SOME (RealNumber (Real.fromInt x - y))  
    | (SOME (RealNumber x), SOME (Integer y)) => SOME (RealNumber (x - Real.fromInt y))  
    | _ => NONE)
  | eval (Mul(x, y), env) =
    (case (eval(x, env), eval(y, env)) of
      (SOME (Integer x), SOME (Integer y)) => SOME (Integer (x * y))
    | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber (x * y))
    | (SOME (Integer x), SOME (RealNumber y)) => SOME (RealNumber (Real.fromInt x * y))  
    | (SOME (RealNumber x), SOME (Integer y)) => SOME (RealNumber (x * Real.fromInt y))  
    | _ => NONE)
  | eval (Div(x, y), env) =
    (case (eval(x, env), eval(y, env)) of
      (SOME (Integer x), SOME (Integer y)) => SOME (RealNumber (Real.fromInt x / Real.fromInt y))
    | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (RealNumber (x / y))
    | (SOME (Integer x), SOME (RealNumber y)) => SOME (RealNumber (Real.fromInt x / y))  
    | (SOME (RealNumber x), SOME (Integer y)) => SOME (RealNumber (x / Real.fromInt y))  
    | _ => NONE)
  | eval (Mod(x, y), env) =
    (case (eval(x, env), eval(y, env)) of
      (SOME (Integer x), SOME (Integer y)) => SOME (Integer ( x mod y))
    | (SOME (RealNumber x), SOME (RealNumber y)) => SOME (Integer (Real.trunc x mod  Real.trunc y))
    | (SOME (Integer x), SOME (RealNumber y)) => SOME (Integer (x + Real.trunc y))  
    | (SOME (RealNumber x), SOME (Integer y)) => SOME (Integer (Real.trunc x + y))  
    | _ => NONE)
  | eval (Let(var, exp, scope), env) =
    let
      val new_env = env @ [VariableBinding(var, (exp, env))]
    in
      eval(scope, new_env)
    end
  | eval(Lambda(arg, body), env) = SOME (Function(arg, body, env))
  | eval(Fun(name, vars, exp, exp'), env) =
    eval(Let(name, fromFunToLambda(vars, exp), exp'), env)
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
end
(* val program = "let x = 5.3 in let y = \\z -> z in (y 5)" *)
(* val program = "let y = 5 in let y = x + x in let x = 10 in y" *)
(* val program = "let y = 10 + y in(y 4)" *)
(* val program = "let x = \\y-> 5 in 5" *)
(* val program = "id x = x" *)

fun main () =
    let
      val userInput = TextIO.inputLine TextIO.stdIn
    in
      case userInput of
          NONE => "No input"  (* In case of no input or end of stream *)
         | SOME str => 
          let
            val program = str
            val tokens = Lexemes.tokenize (String.explode program)
            val (ast, _) = Parser.parser tokens
          in
            TextIO.print(Parser.toString(Parser.eval(ast, [])) ^ "\n");
            main () 
          end
    end;
main()

(* val program = "let y = (\\x->(x x) \\x->(x x)) in 5" *)
(* val program = "let y = (\\x->(x x) \\x->(x x)) in y" *)
(* val program = "let x = \\a->\\b->\\c-> a + b + c in (((x 5) 6) 7) + 1" *)
(* val program = "let y = add x = x+1  in (x y) in (y 5)" *)
(* val program = "let y = (\\x->(x x) \\x->(x x)) in y" *)
(* val program = "let y = \\x->5 in y" *)
(* val program = "add a b = a + b" *)
(* val program = "((x 5) 6)" *)
(* val program = "\\a->\\b-> a + b" *)
(* val scoop =Let("y", ConstInt 5, Let("y", Plus(Var "x", Var "x"), Let("x", ConstInt 10, Var "y"))) *)
(* val program = "add x y = x + y " *)
(* val program = "let x = if 1 == 1 && 2 < 3 && 3 < 1 then 1 else 0 in x" *)
(* val program = "let x = 2 in x + 1" *)
(* val program = "let x = 0 - 2 / 1.5 in (x)" *)
(* val program = "add x y = x + y" *)
(* val program = "((add x y = x + y) 6)" *)
(* val program = "((add x y = x + y) 6) 8)" *)
(* val program = "let z = (((add x y = x + y) 6) 7) in z" *)
(* val program = "(let fact = if n == 1 then 1 else n * (fact n-1) in fact))" *)
(* val program = "let x = if n == 1 then 1 else 3-1 * 2 in x" *)
(* val program = "fact n = if n == 1 then 1 else n * (fact (n-1)) " *)
(* val program = "fact n = if n == 1 then 1 else n * (fact (n-1)) in (fact 5)" *)
(* val program = "let fact = \\n-> if n == 1 then 1 else n * (fact (n-1)) in (fact 5)" *)
(* val program = "let fact n = if n == 1 then 1 else n * (fact (n-1)) in fact 2" *)
(* val program = "let fact n = if n == 1 then 1 else n * (fact (n-1)) in fact 2" *)
(* val program = "if 0 < 2 && 1 < 2  && 0 > 1 then 1 else 0" *)
(* val program = "let x = let y = 2 in y + 1 in x + y" *)

(* val tokens = Lexemes.tokenize (String.explode program)
val (ast, _) = Parser.parser tokens
val print = Parser.eval(ast, [])  *)

(* val env = [] : Parser.Entry list
val new_env = env @ [Parser.VariableBinding ("a", (Parser.HaskellType(Parser.Integer 5), []))] *)

