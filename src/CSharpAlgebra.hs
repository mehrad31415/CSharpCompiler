{-# LANGUAGE RecordWildCards #-}
module CSharpAlgebra where

import CSharpLex
import CSharpGram

{-
  Only modify this file when you change the AST in CSharpGram.hs
-}

data CSharpAlgebra c m s e
  = CSharpAlgebra
      { clas :: String -> [m] -> c

      , memberD :: Decl -> m
      , memberM :: Type -> String -> [Decl] -> s  -> m

      , statDecl   :: Decl -> s
      , statExpr   :: e -> s
      , statIf     :: e -> s -> s  -> s
      , statWhile  :: e -> s -> s
      , statReturn :: e -> s
      , statBlock  :: [s] -> s

      , exprConst :: Int -> e
      , exprVar   :: String -> e
      , exprOper  :: String -> e -> e  -> e
      , exprMeth  :: String -> [e] -> e
      }

-- foldCSharp :: CSharpAlgebra clas s e -> Class -> clas
-- foldCSharp (CSharpAlgebra c,  md, mm,  sd, se, si, sw, sr, sb,  ec, ev, eo) = fClas
--   where
--     fClas (Class      t ms)     = c  t (map fMemb ms)
--     fMemb (MemberD    d)        = md d
--     fMemb (MemberM    t m ps s) = mm t m ps (fStat s)
--     fStat (StatDecl   d)        = sd d
--     fStat (StatExpr   e)        = se (fExpr e)
--     fStat (StatIf     e s1 s2)  = si (fExpr e) (fStat s1) (fStat s2)
--     fStat (StatWhile  e s1)     = sw (fExpr e) (fStat s1)
--     fStat (StatReturn e)        = sr (fExpr e)
--     fStat (StatBlock  ss)       = sb (map fStat ss)
--     fExpr (ExprConst  con)      = ec con
--     fExpr (ExprVar    var)      = ev var
--     fExpr (ExprOper   op e1 e2) = eo op (fExpr e1) (fExpr e2)

-- The "{..}" notation brings all fields of the algebra into scope.
-- This means that, for example, 'memberD' in the 'fMemb' definition 
-- refers to the 'memberD' field of the given algebra.
foldCSharp :: CSharpAlgebra c m s e -> Class -> c
foldCSharp CSharpAlgebra{..} = fClas
  where
    fClas (Class      t ms)     = clas t (map fMemb ms)
    fMemb (MemberD    d)        = memberD d
    fMemb (MemberM    t m ps s) = memberM t m ps (fStat s)
    fStat (StatDecl   d)        = statDecl d
    fStat (StatExpr   e)        = statExpr (fExpr e)
    fStat (StatIf     e s1 s2)  = statIf (fExpr e) (fStat s1) (fStat s2)
    fStat (StatWhile  e s1)     = statWhile (fExpr e) (fStat s1)
    fStat (StatReturn e)        = statReturn (fExpr e)
    fStat (StatBlock  ss)       = statBlock (map fStat ss)
    fExpr (ExprConst  con)      = exprConst con
    fExpr (ExprVar    var)      = exprVar var
    fExpr (ExprOper   op e1 e2) = exprOper op (fExpr e1) (fExpr e2)
    fExpr (ExprMeth s e)        = exprMeth s (map fExpr e)
