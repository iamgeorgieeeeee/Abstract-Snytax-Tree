
> import Parsing
> -- import Data.List.Split
> import Data.Map

Abstract Syntax data type

> type Ident = String

> data AExp
>         = AE AOp AExp AExp
>         | Num Int
>         | Neg AExp 
>         | Var Ident
> --          | Assign Ident AExp
>           deriving Show

> data AOp = Add | Mul | Sub | Div deriving Show


Concrete Syntax

E = T "+" E   {return AE Add t e} 
  | T "-" E   {return AE Sub t e}
  | T

> expr :: Parser AExp
> expr = 
>     (do t <- term
>         symbol "+"
>         e <- expr 
>         return (AE Add t e)
>     ) 
>     +++ ((do t <- term; symbol "-"; e <- expr; return (AE Sub t e)))
>     +++ term 

T = F "*" T 
  | F "/" T 
  | F

> term :: Parser AExp
> term = (do f <- factor; symbol "*"; t <- term; return (AE Mul f t)) 
>  +++ (do f <- factor; symbol "/"; t <- term; return (AE Div f t)) 
>  +++ factor 


F = (E)
  | - E
  | N 
  | V

> factor :: Parser AExp
> factor = 
>    (do symbol "("; e <- expr; symbol ")"; return e) 
>    +++ (do symbol "-"; e <- expr; return (Neg e))
>    +++ number
>    +++ variable


N = D | DN
D = 0 | 1 | 2 | .. | 9

Examples:

3
2 * 5
2 + 3 * 7
(2 + 3) * 7



<|>



E = E + E
  | E - E
  | E * E
  | E / E


2 * 3 + 4


    *
   / \
  2   +
     / \
    3   4

    +
   / \
  *   4
 / \
2   3

> number :: Parser AExp
> number = do n <- natural; return (Num n)

> variable :: Parser AExp
> variable = do id <- identifier; return (Var id)

> parse_exp :: String -> AExp
> parse_exp str = case parse expr str of
>                            [(ast,[])] -> ast
>                            [(_,out)]  -> error ("unused input " ++ out)
>                            []         -> error "invalid input"

-- rel 
--  = e1 == e2
--  | e1 < e2
--  | e1 > e2
--  | e1 <= e2
--  | e1 >= e2


-- e1 relop e2

-- > data RExp
-- >    = Equal AExp AExp
-- >    | Less AExp AExp
-- >    | Greater AExp AExp
-- >    | LessE AExp AExp
-- >    | GreaterE AExp AExp
-- >    deriving Show


evalrel :: RExp -> Bool

evalrel (Less (Num 1)(Num 2)) ==>  eval (Num 1) < eval (Num 2)
                              ==>  1 < 2
                              ==> True

> type Postfix = String
> type Infix = String
> type Binding = (Ident, Int)
> type Env = [Binding]

> to_postfix :: AExp -> Postfix
> to_postfix (Num n)   = show n
> to_postfix (AE Add t u) = to_postfix t ++ " "++ to_postfix u ++ " " ++ "+"
> to_postfix (AE Mul t u) = to_postfix t ++ " "++ to_postfix u ++ " " ++ "*"

> postfix :: Infix -> Postfix
> postfix = to_postfix . parse_exp


> to_infix :: AExp -> Infix
> to_infix (Num n) = show n
> to_infix (AE Add t u) = "(" ++ to_infix t ++ "+" ++ to_infix u ++ ")"
> to_infix (AE Mul t u) = "(" ++ to_infix t ++ "*" ++ to_infix u ++ ")"


> to_prefix :: AExp -> String
> to_prefix (Num n)   = show n
> to_prefix (AE Add t u) = "+ " ++ to_prefix t ++ " " ++ to_prefix u
> to_prefix (AE Mul t u) = "* " ++ to_prefix t ++ " " ++ to_prefix u


haskell platform


> eval :: AExp -> Int
> eval (Num n) = n
> eval (AE Add t u) = (eval t) + (eval u)
> eval (AE Mul t u) = (eval t) * (eval u)
> eval (AE Sub t u) = (eval t) - (eval u)


+ * 2 3 6



interp "2 * 3 + 6" == 12
interp "2 * (3 + 6) == 18


       +
      / \
     *   6
    / \
   2   3


x:= 3; y := 1; while not (x == 1) do (y := y *x; x := x - 1)

                   ;
               /  |    \
              := :=     while
             / \ / \    /   \
            x  3 y 1   not    ;
                       |    / \
                      ==   :=  :=
                     /  \  / \ / \
                    x    1 y * x  -
                             /\   /\
                            y  x  x 1

> interp :: String -> Int
> interp = eval . parse_exp


interp str = eval (parse_exp str)


f : B -> C
g : A -> B

h : A -> C
h : f o g  

h = f o g



Compilation
-----------
- translations of expression to sequences of instructions
  (reverse Polish notation)


what we did in parsing:
 
|n|     = Num n
|t + u| = Add |t| |u|
|t - u| = Sub |t| |u|
|t * u| = Mul |t| |u|
|t / u| = Div |t| |u|



|(1+2)*3| = Mul |1+2| |3|
          = Mul (Add |1| |2|) (Num 3)
          = Mul (Add (Num 1) (Num 2)) (Num 3)


Translation to machine instructions
-----------------------------------
Machine instructions: 

    ins = VAL n | ADD | SUB | MUL | DIV

VAL n - push the value n onto the stack
ADD   - pop two values from the stack, add them and push the result into the stack
MUL   - pop two values from the stack, multiply them and push the result into the stack

-- > data Ins = VAL Int | ADD | SUB | MUL | DIV deriving Show
-- > type Code = [Ins]
 
Translation rules (compilation)

C : Abstract Syntax -> Code

C |n|     = VAL n
C |t + u| = C |t| ; C |u| ; ADD 
C |t - u| = C |t| ; C |u| ; SUB
C |t * u| = C |t| ; C |u| ; MUL
C |t / u| = C |t| ; C |u| ; DIV


> to_code :: AExp -> Code
> to_code (Num n)   = [VAL n]
> to_code (AE Add t u) = to_code t ++ to_code u ++ [ADD]
> to_code (AE Sub t u) = to_code t ++ to_code u ++ [SUB]
> to_code (AE Mul t u) = to_code t ++ to_code u ++ [MUL]
> to_code (AE Div t u) = to_code t ++ to_code u ++ [DIV]






C |(1+2)*3| = C (Mul (Add (Num 1) (Num 2)) (Num 3)) 
                 
            = C (Add (Num 1) (Num 2)); VAL 3; MUL
            = [VAl 1,VAl 2, ADD, VAL 3, MUL]

> compile :: String -> Code
> compile  = to_code . parse_exp

Operational Semantics (evaluation of expression using the machine)
(expressed as state transitions)

Components of the machine:
1. Code Segment (list of ins)
2. Stack (list of values)

represented by a pair : (code, stack)

Initial State: 
                code  = C |a|
                stack = empty

Final State  : 
                code  = empty
                stack = result is in the top of the stack

State transitions rules:

Machine state before       Machine state after
-----------------------    -------------------

( [CONST n; c], s )     = ( c, [n, s] )
( [ADD; c], [m, n, s] ) = ( c, [r, s) ) where r = n + m
( [SUB; c], [m, n, s] ) = ( c, [r, s] ) where r = n - m
( [MUL; c], [m, n, s] ) = ( c, [r, s] ) where r = n * m
( [DIV; c], [m, n, s] ) = ( c, [r, s] ) where r = n / m
( [], s])               = ([], s])

> type Stack = [Int]
> type State = (Code, Stack)

> meval :: State -> State
> meval ((VAL n : c), s )    = (c, (n : s))
> meval ((ADD : c), (m:n:s)) = (c, (r : s)) where r = n + m
> meval ((SUB : c), (m:n:s)) = (c, (r : s)) where r = n - m
> meval ((MUL : c), (m:n:s)) = (c, (r : s)) where r = n * m
> meval ((DIV : c), (m:n:s)) = (c, (r : s)) where r = n `div` m
> meval ([], s)              = ([], s)

> initstate :: Code -> State
> initstate code = (code, [])

> exec :: String -> Int
> exec exp  = res
>   where 
>       (_,[res]) = meval $ initstate $ compile exp


c   = skip                      Skip 
    | x := a                    Assign x a
    | c1; c2                    Seq c1 c2
    | if b then c1 else c2      If b c1 c2
    | while b do c              While b c

 data Com
   = Skip
   | Assign String AExp
   | Seq Com Com
   | If BExp Com Com
   | While BExp Com

Example

c1; c2; c3 ==  (c1; c2); c3 == c1; (c2; c3) 

1 - 2 - 3  == (1 - 2) - 3 == 1 - (2 - 3)

Seq (Seq c1 c2) c3

       ;
      / \
     ;  c3
    / \
   c1  c2
Seq c1 (Seq c2 c3)

      ;
     / \
    c1  ;
       / \
      c2  c3


x := 1; y := 2
==> Seq (Assign "x" (Num 1)) (Assign "y" (Num 2))


> equal :: Parser BExp
> equal = do e1 <- expr; symbol "=="; e2 <- expr; return (RE Equal e1 e2)

> less :: Parser BExp
> less = do e1 <- expr; symbol "<"; e2 <- expr; return (RE Less e1 e2)

> lessE :: Parser BExp
> lessE = do e1 <- expr; symbol "<="; e2 <- expr; return (RE LessE e1 e2)

> greater :: Parser BExp
> greater = do e1 <- expr; symbol ">"; e2 <- expr; return (RE Greater e1 e2)

> greaterE :: Parser BExp
> greaterE = do e1 <- expr; symbol "<="; e2 <- expr; return (RE GreaterE e1 e2)

> rexp :: Parser BExp
> rexp = equal +++ less +++ lessE +++ greater +++ greaterE

parse rexp "(1 + 2) < (3 * 2)"
=> Less (Add (Num 1)(Num 2)) (Mul (Num 3)(Num 2))

> parse_rexp :: String -> BExp
> parse_rexp str = case parse rexp str of
>                            [(ast,[])] -> ast
>                            [(_,out)]  -> error ("unused input " ++ out)
>                            []         -> error "invalid input"


-- > LExp = BVal True 
-- >      | Rel RExp
-- >      | Not LExp
-- >      | And LExp LExp
-- >      | Or LExp LExp

> data BExp
>    = Not BExp
>    | Rel BExp
>    | BVal Bool
>    | BVar Ident
>    | LE LOp BExp BExp
>    | RE ROp AExp AExp
>    deriving Show

> data LOp = And | Or deriving Show

> data ROp = Equal | Less | LessE | Greater | GreaterE deriving Show

data Exp = B BExp | A AExp

> bexp :: Parser BExp
> bexp =
>   do
>     bt <- bterm
>     symbol "|"
>     b <- bexp
>     return (LE Or bt b)
> 
>   +++bterm

> bterm :: Parser BExp
> bterm = (do bf <- bfactor; symbol "&"; bt <- bterm; return (LE And bf bt)) +++ bfactor

> bfactor :: Parser BExp
> bfactor = (do symbol "!"; bv <- bval; return (Not bv)) +++ bval

> bval :: Parser BExp
> bval = (do symbol "("; b <- bexp; symbol ")"; return b) +++ (do symbol "T"; return (BVal True)) +++ (do symbol "F"; return (BVal False)) +++ (do rel <- rexp; return (Rel rel)) +++ (do bvar <- identifier; return (BVar bvar))

> parse_bexp :: String -> BExp
> parse_bexp bstr = 
>      case parse bexp bstr of
>          [(b, "")] -> b
>          [(_, s)]  -> error "invalid input"
>          []        -> error "invalid input"

> beval :: BExp -> Bool
> beval (BVal b)    = b
> beval (LE Or b1 b2)  = beval b1 || beval b2
> beval (LE And b1 b2) = beval b1 && beval b2
> beval (Not b)     = not (beval b)

> resolve :: String -> Bool
> resolve = beval . parse_bexp

> data Com 
>    = Skip
>    | Assign Ident AExp
>    | Seq Com Com
>    | If BExp Com Com
>    | While BExp Com
>    deriving Show


parse_while "if x == 1 then x := 1 else skip"
==> If ((Equal) (Var "x") (Num 1)) (Assign "x" (Num 1)) Skip

parse_while "x := 0; while x <= 1 do x := x + 1"
==> Seq (Assign "x" (Num 0)) (While (LessE (Var "x") (Num 1)) (Assign "x" (Add (Var "x") (Num 1))))
==> Seq [Assign "x" (Num 0)), While (LessE (Var "x") (Num 1)) (Assign "x" (Add (Var "x") (Num 1))]

x := 1;skip ==> Seq (Assign "x" (Num 1)) Skip
            ==> Seq [(Assign "x" (Num 1)), Skip]

x := 1; y := 0; skip
==> Seq (Seq (Assign "x" (Num 1)) (Assign "y" (Num 0))) Skip
--> Seq [Assign "x" (Num 1), Assign "y" (Num 0), Skip]

c1; c2; c3 => Seq (Seq c1 c2) c3
c1; c2; c3; c4 => Seq (Seq (Seq c1 c2) c3) c4

> skipCmd :: Parser Com
> skipCmd = do c <- symbol "skip"; return Skip

> assignCmd :: Parser Com
> assignCmd = (do bf <- identifier; symbol ":="; bt <- expr; return (Assign bf bt))

> ifCmd :: Parser Com
> ifCmd = do symbol "if"; b <- bexp; symbol "then"; bt <- stmtSeq; symbol "else"; bw <- stmtSeq; return (If b bt bw)

> whileCmd :: Parser Com
> whileCmd =  do symbol "while"; b <- bexp; symbol "do"; bt <- stmtSeq; return (While b bt) 

> cmd :: Parser Com
> cmd = skipCmd +++ assignCmd +++ ifCmd +++ whileCmd

> cmdParen :: Parser Com
> cmdParen = do symbol "("; c <- stmtSeq; symbol ")"; return c

> com :: Parser Com
> com = cmd +++ cmdParen

> parse_while :: String -> Com
> parse_while str = 
>      case parse stmtSeq str of
>          [(ast, [])] -> ast
>          [(_, out)]  -> error ("invalid input" ++ out)
>          []          -> error "invalid input"

-- > seqOfStatment :: Parser Com
-- > seqOfStatment = 
-- >          do
-- >            lst <- splitAt ";" map parse_while lst
-- >             if (length lst == 1)
-- >               then return (head lst)
-- >             else return (Seq lst)

stmtSeq :: Parser Com
stmtSeq = do c1 <- com; symbol ";"; c2 <- com; return (Seq ([c1,c2]))

> stmtSeq :: Parser Com
> stmtSeq = do c1 <- com
>              do symbol ";"
>                 c2 <- stmtSeq
>                 return (Seq c1 c2)
>                 +++ return c1





> type Code = [Ins]
> data Ins
>      = PUSH Int
>      | VAL Int
>      | DIV
>      | ADD
>      | MUL
>      | SUB
>      | TRUE
>      | FALSE
>      | EQUAL
>      | GREATER
>      | LESS
>      | AND
>      | OR
>      | NEG
>      | LOAD String
>      | STORE String
>      | NOOP
>      | BRANCH Code Code
>      | LOOP Code Code
>      | GTE
>      | LTE
>      deriving Show

Translator for arithmetic Expressions

> transA :: AExp -> Code
> transA (Num z)     = [PUSH z]
> transA (Var x)     = [LOAD x]
> transA (AE Add a1 a2) = transA a2 ++ transA a1 ++ [ADD]
> transA (AE Sub a1 a2) = transA a2 ++ transA a1 ++ [SUB]
> transA (AE Mul a1 a2) = transA a2 ++ transA a1 ++ [MUL]


Translator for Boolean Expression

> transB :: BExp -> Code
> transB (BVal True)         = [TRUE]
> transB (BVal False)        = [FALSE]
> transB (Not a1)            = transB a1 ++ [NEG]
> transB (LE And a1 a2)      = transB a2 ++ transB a1 ++ [AND]
> transB (LE Or a1 a2)       = transB a2 ++ transB a1 ++ [OR]
> transB (RE Equal a1 a2)    = transA a2 ++ transA a1 ++ [EQUAL] 
> transB (RE Greater a1 a2)  = transA a2 ++ transA a1 ++ [GREATER]
> transB (RE GreaterE a1 a2) = transA a2 ++ transA a1 ++ [GTE]
> transB (RE Less a1 a2)     = transA a2 ++ transA a1 ++ [LESS]
> transB (RE LessE a1 a2)    = transA a2 ++ transA a1 ++ [LTE]

Translator for Translation Statements

> transC :: Com -> Code
> transC (Skip)         = [NOOP]
> transC (Assign a1 a2) = transA a2 ++ [STORE a1]
> transC (Seq a1 cs)    = transC a1 ++ transC cs
> transC (If b a1 a2)   = transB b ++ [BRANCH (transC a1) (transC a2)]
> transC (While a1 a2)  = [LOOP (transB a1) (transC a2)]


Map

> data ValZB = Z Int | TF Bool
> type StackNew = [ValZB]
> type BindingNEW = (String, Int)
> type Mem = [BindingNEW] 
> type Mstate = (Code, StackNew, Mem)

> initState :: Code -> Mstate
> initState codes = (codes, [], [])

> mevalNew :: Mstate -> Mstate
> mevalNew (PUSH n : codes, stack, mem) = (codes,(Z n) : stack, mem)
> mevalNew (ADD:codes, (Z n1):(Z n2):stack, mem) = (codes, (Z (n1+n2)):stack, mem)
> mevalNew (SUB:codes, (Z n1):(Z n2):stack, mem) = (codes, (Z (n1-n2)):stack, mem)
> mevalNew (MUL:codes, (Z n1):(Z n2):stack, mem) = (codes, (Z (n1*n2)):stack, mem)
> mevalNew (EQUAL:codes, (TF n1):(TF n2):stack, mem) = (codes, (TF (n1==n2)):stack, mem)
> mevalNew (GREATER:codes, (TF n1):(TF n2):stack, mem) = (codes, (TF (n1<n2)):stack, mem)
> mevalNew (GTE:codes, (TF n1):(TF n2):stack, mem) = (codes, (TF (n1>=n2)):stack, mem)
> mevalNew (LESS:codes, (TF n1):(TF n2):stack, mem) = (codes, (TF (n1<n2)):stack, mem)
> mevalNew (LTE:codes, (TF n1):(TF n2):stack, mem) = (codes, (TF (n1<=n2)):stack, mem)
> mevalNew ([], stack, mem) = ([], stack, mem)
> mevalNew (NEG: codes, (TF n1):stack,mem) = (codes, (TF (not (n1))):stack,mem)
> mevalNew (NOOP:codes, stack, mem) = (codes, stack, mem)
> mevalNew (LOAD x:codes, stack, mem) = (codes, mem x :stack, mem)



-- valueOf :: String -> Storage -> Integer
-- valueOf = 
-- valueOf :: Mem -> String -> Int

-- addOrReplace :: Mem -> String -> Int -> Mem

-- > type Stack = [Int]
-- > type State = (Code, Stack)
