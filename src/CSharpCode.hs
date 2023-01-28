module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Data.Maybe
{-
  This file contains a starting point for the code generation.
-}

-- The S (Statement) data type has been given a universal environment [(k,v)] (in the form of Map k v) 
-- which is a list of all the declared variables and their corresponding values. 
-- in the case of M (method): any declared variable in a method can only be used inside a method and is added to the environment for the duration of the method.
type Env = M.Map String Int
(?) :: Env -> String -> Maybe Int
(?) env x = M.lookup x env -- equivalent to flip M.lookup
-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                          -- Class
type M = Code                          -- Member
type S = Env -> (Code, Env)            -- Statement
type E = ValueOrAddress -> Env -> Code -- Expression


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprCon
  fExprVar
  fExprOp
  fExprMeth


fClass :: String -> [M] -> C -- String -> [[Instr]] -> [Instr]
fClass c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> M -- Decl -> Code
fMembDecl d = []

-- each declaration made inside a method can be used ONLY inside that method, thus the environment must be updated within that scope.
-- the defined variables must be loaded onto the stack.
fMembMeth :: Type -> String -> [Decl] -> S -> M
-- Type -> String -> [Decl] -> Env -> (Code, Env) -> Code
fMembMeth t x ps s = [LABEL x ,LDR MP, LDRR MP SP] ++ map l ps ++ fst (s env) ++ [LDRR SP MP,STR MP, RET] 
  where env = g ps 1
        g :: [Decl] -> Int-> Env 
        g [] _ = M.empty
        g (Decl _ p: ps) i  =  M.insert p i (g ps (i + 1)) -- the declarations inside the scope of the method are added to the environment.
        l _ = LDS (-(1 + M.size env))

-- every declaration in a statement is added to the environment.
-- note that only the value of the variable is inserted and the type is not stored in the environment.
fStatDecl :: Decl -> S -- Decl -> Env -> (Code, Env) 
fStatDecl (Decl _ s) env = ([AJS 1], M.insert s (M.size env + 1) env)


fStatExpr :: E -> S -- ValueOrAddress -> Env -> Code -> Env -> (Code, Env)
fStatExpr e env = (e Value env ++ [pop], env)

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = (e Value env ++ [BRF (a1 + 2)] ++ fst (s1 env) ++ [BRA a2] ++ fst (s2 env), env)
    where a1 = codeSize $ fst (s1 env)
          a2 = codeSize $ fst (s2 env)

fStatWhile :: E -> S -> S
fStatWhile e s1 env =( [BRA a1] ++ fst (s1 env) ++ e Value env ++ [BRT (-(a1 + a2 + 2))], env)
    where a1 = codeSize $ fst (s1 env)
          a2 = codeSize (e Value env)

fStatReturn :: E -> S
fStatReturn e env = (e Value env ++ [STR RR, pop] ++ [LDRR SP MP,STR MP,RET],env)

-- what is defined in a block stays in a block; in other words, returning outside the block, the environment returns to the initial state when entering that block. Nonetheless, the environment is updated inside the block.
-- inside the block all the variables in the environment (from outer scope) can be used. 
fStatBlock :: [S] -> S
fStatBlock ss env = (sb ss env, env)
  where sb :: [S] -> Env -> Code
        sb [] env' = [STS (- n) ,AJS (- (n- 1) )] 
            where n = M.size env' - M.size env -- returning the environment to the initial size.
        sb (s:ss') env' =  fst (s env') ++ sb ss' (snd (s env'))

-- int, boolean, and characters.
fExprCon :: Int -> E
fExprCon n va env = [LDC n]

fExprVar :: String -> E
fExprVar x Value env   = [LDL ( env M.! x)]
fExprVar x Address env = [LDLA ( env M.! x)]

fExprMeth :: String -> [E] -> E -- String -> [ValueOrAddress -> Env -> Code] -> ValueOrAddress -> Env -> Code
-- ValueOrAddress -> Env -> Code 
-- print is a function call
fExprMeth "print" es va env = concatMap (\x -> x Value env ++ [TRAP 0]) es ++ [LDC 0]
fExprMeth s es va env =  concatMap (\x -> x Value env) es ++ [Bsr s ,LDR RR] 

fExprOp :: String -> E -> E -> E
-- logical operators (&&) and (||) are computed lazy.
-- laziness in evaluation of && means that if the first boolean value is False do not calculate the second value and give the result False. In other words, only calculate the second value of the first one is True.
-- laziness in evaluation of || means that if the first boolean is True do not evaluate the second boolean.
fExprOp "="  e1 e2 va env  = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp "&&" e1 e2 va env  = 
  e1 Value env ++                           -- expression one is evaluated
  [BRF (codeSize (e2 Value env)+2)] ++      -- it false jump the next instruction and jump BRT and load 0.
  e2 Value env ++                           -- if true then expression two is evaluated
  [BRT 4] ++                                -- if true jump then next two instructions and load 1
  [LDC 0, BRA 2] ++                         -- if False Load 0 and jump the next instruction of size 2
  [LDC 1] ++                                -- loads 1 if true
  [AND]                                     -- The AND operation
fExprOp "||" e1 e2 va env  = 
  e1 Value env ++                           -- first expression one is evaluated.
  [BRT (codeSize (e2 Value env)+ 6)] ++     -- if true we jump the next expression and the next three instructions (each of size 2) (we will be at LDC 1)
  e2 Value env ++                           -- if not true then expression two is evaluated
  [BRT 4] ++                                -- if true jump then next two instructions and load 1
  [LDC 0, BRA 2] ++                         -- if False Load 0 and jump the next instruction of size 2
  [LDC 1] ++                                -- loads 1 if true
  [OR]                                      -- The OR operation
fExprOp  op  e1 e2 va env  = e1 Value env ++ e2 Value env ++ [fromJust (M.lookup op opCodes)]
  where
    -- list of the other 12 operators which are handled strictly.
    opCodes :: M.Map String Instr
    opCodes = M.fromList [ ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD)
                          , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ)
                          , ("!=", NE), ("^", XOR)
                          ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
