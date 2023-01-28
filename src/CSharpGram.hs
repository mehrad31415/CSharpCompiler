module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Int
          | ExprVar    String
          | ExprMeth   String [Expr]  -- adding the possibility to “call a method with parameters” to the syntax of expressions
          | ExprOper   String Expr Expr
          deriving Show

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl sComma) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr   <$> pExpr           <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> statFor
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> parenthesised pExpr
           <|> greedyChoice [ExprMeth <$> sLowerId <*> parenthesised (option (listOf pExpr sComma) []), ExprVar <$> sLowerId] -- first the expression is dealt as it is a function application and then if not possible a variable, thats why we have used greedy choice. If we did not do that the a function application might interpreted as a variable with the POpen token in front of it and the rest...
{-
priority of operators: 
1. *, /, % 
2. -, +
3. <=, <, >=, >
4. !=, ==
5. ^
6. &&
7. ||
8. =
-}
-- besides the "=" operator all other 14 operators are left-associative.
-- the "=" operator is right associative and has the lowest precedence as well.
-- parse pExpr [ConstInt 1,Operator "+",ConstInt 2, Operator "*", ConstInt 1] 
-- example: 1 + 2 * 1
pExpr :: Parser Token Expr
pExpr = chainr expr ((\(Operator x) -> ExprOper x) <$> symbol (Operator "="))

-- we list the other 14 operators from the lowest precedence to the highest precendence with foldr.
-- the gens function does a left-associative chain starting from the last operator which in turn has then highest precedence
-- for further information on the definition below please refer to section 3.6 of the lecture notes.
type Op a = (Token, a -> a -> a)
gen :: [Op a] -> Parser Token a -> Parser Token a 
gen ops p = chainl p (choice (map f ops))
  where f (s,c) = c <$ symbol s

expr :: Parser Token Expr
expr = foldr gen pExprSimple [group7 , group6, group5, group4, group3, group2, group1]

group7 :: [(Token, Expr -> Expr -> Expr)]
group7 = [(Operator "||", ExprOper "||")]
group6 :: [(Token, Expr -> Expr -> Expr)]
group6 = [(Operator "&&", ExprOper "&&")]
group5 :: [(Token, Expr -> Expr -> Expr)]
group5 = [(Operator "^", ExprOper "^")]
group4 :: [(Token, Expr -> Expr -> Expr)]
group4 = [(Operator "!=", ExprOper "!="),(Operator "==", ExprOper "==")]
group3 :: [(Token, Expr -> Expr -> Expr)]
group3 = [(Operator "<=", ExprOper "<="),(Operator ">=", ExprOper ">="), (Operator "<", ExprOper "<"), (Operator ">", ExprOper ">")]
group2 :: [(Token, Expr -> Expr -> Expr)]
group2 = [(Operator "+", ExprOper "+"),(Operator "-", ExprOper "-")]
group1 :: [(Token, Expr -> Expr -> Expr)]
group1 = [(Operator "*", ExprOper "*"),(Operator "/", ExprOper "/"), (Operator "%", ExprOper "%")]

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType
     <|> TypeObj  <$> sUpperId

-- writing the parser for "for" statements.
-- the "for" statement has 4 non-terminals on the right hand side of the production rule.
statFor :: Parser Token Stat
statFor = ( \a1 a2 a3 a4 a5 a6 a7 a8 a9 ->
     StatBlock (f a3 ++ [StatWhile a5 (StatBlock (a9 : f a7))]))
      <$> symbol KeyFor 
      <*> symbol POpen
      <*> option pExprDecl ExprDeclsNothing
      <*> sSemi
      <*> pExpr
      <*> sSemi
      <*> option pExprDecl ExprDeclsNothing
      <*> symbol PClose
      <*> pStat

-- to write the parser we define the ExprDecls data type which corresponds to the exprdecls non terminal in the production rule.
data ExprDecls = 
     ForExpr Expr
   | ForDecl Decl
   | ForExprs Expr ExprDecls 
   | ForDecls Decl ExprDecls 
   | ExprDeclsNothing -- this constrcutor is given for the default value of the option function.
     deriving Show
-- we write a parser for this new data type.
pExprDecl :: Parser Token ExprDecls
pExprDecl = ForExpr  <$> pExpr
       <|>  ForDecl  <$> pDecl
       <|>  ForExprs <$> pExpr <* symbol Comma <*> pExprDecl
       <|>  ForDecls <$> pDecl <* symbol Comma <*> pExprDecl

-- example: for (int i ,i = 0; i < 5; i = i + 1) {}
-- example running interactively: parse statFor [KeyFor, POpen, StdType "int", LowerId "i", Comma, LowerId "i", Operator "=", ConstInt 0, Semicolon, LowerId "i", Operator "<", ConstInt 5, Semicolon, LowerId "i", Operator "=", LowerId "i", Operator "+", ConstInt 1, PClose, COpen, CClose]  
-- int i, i = 0 are declrarations and expressions inside a "for" statement which inside a "while" loop come before it.
-- therefore we parse them before the while loop. To do so we need to turn the whole loop into a StatBlock.
-- where int i, i = 0 are stats (declaration or expression)
-- i = i + 1 will become a stat inside the while block
-- i<5 is the stat for the StatWhile
-- to implement this we need a function which converts ExprDecls to Stat (here a list of Stat to make it compatible with the field of StatBlock)
f :: ExprDecls -> [Stat] 
f ExprDeclsNothing      =  []
f (ForExpr expr)        = [StatExpr expr]
f (ForDecl decl)        = [StatDecl decl]
f (ForDecls decl decls) = StatDecl decl : f decls
f (ForExprs expr exprs) = StatExpr expr : f exprs

-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}
