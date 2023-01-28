module CSharpLex where

import Data.Char
import Control.Monad (guard)
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>), sequence)
import Data.Maybe

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyFor                 -- extended the compiler to support a "for" statement (5)
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace) <* eof

lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexLowerId
             , lexUpperId
             ]


lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ ( POpen     , "("      )
      , ( PClose    , ")"      )
      , ( SOpen     , "["      )
      , ( SClose    , "]"      )
      , ( COpen     , "{"      )
      , ( CClose    , "}"      )
      , ( Comma     , ","      )
      , ( Semicolon , ";"      )
      , ( KeyIf     , "if"     )
      , ( KeyElse   , "else"   )
      , ( KeyWhile  , "while"  )
      , ( KeyReturn , "return" )
      , ( KeyTry    , "try"    )
      , ( KeyCatch  , "catch"  )
      , ( KeyClass  , "class"  )
      , ( KeyVoid   , "void"   )
      , ( KeyFor    , "for"    )
      ]


lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]
operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

lexConstInt :: Parser Char Token
lexConstInt = choice [
              ConstInt . read <$> greedy1 (satisfy isDigit)
            , lexConstBool
            , lexConstChar ]

-- given that Char and Bools have to be changed to integers, internally;
-- I have decided to change them to integers during the lexer part. This way
-- there is no need to define Boolean or Character tokens and they can be dealt
-- as they are integers during the parsing process.
lexConstChar :: Parser Char Token
lexConstChar = ConstInt . ord <$> pack (symbol '\'') anySymbol (symbol '\'')

-- true acts as a 1 and false acts as a 0.
lexConstBool :: Parser Char Token
lexConstBool = ConstInt . (\x -> if x == "true" then 1 else 0) <$> choice [token "false", token "true"]

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

-- given that in the main lexer function "lexicalScanner" only white spaces have been discarded,
-- I have decided to add single line comments as a "choice" to the below ("lexWhiteSpace") parser.
-- by doing so, in the result of the main lexer function comments will be discarded too.
-- I wanted to add multi line comments and choose "greedyChoice" for that single line comment has priority over multi line comment, i.e., the string "//*" will be parsed as a single line comment; however, I kept making a mistake somewhere and kept it out.
-- also given that the comments are discarded in lexing there is no need to make them a token for the parsing.

lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (lexSingleComment <|> satisfy isSpace)

lexSingleComment :: Parser Char Char
lexSingleComment = token "//" *> many anySymbol *> choice [symbol '\n', symbol 'r']

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty
----- End Lexer -----


----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = pFromMaybe fromStdType
  where fromStdType (StdType x) = Just x
        fromStdType _           = Nothing

sUpperId :: Parser Token String
sUpperId = pFromMaybe fromUpperId
    where fromUpperId (UpperId x) = Just x
          fromUpperId _           = Nothing

sLowerId :: Parser Token String
sLowerId = pFromMaybe fromLowerId
  where fromLowerId (LowerId x) = Just x
        fromLowerId _           = Nothing

sConst :: Parser Token Int
sConst  = pFromMaybe fromConst
  where fromConst (ConstInt  x) = Just x
        fromConst _             = Nothing

sOperator :: Parser Token String
sOperator = pFromMaybe fromOperator
  where fromOperator (Operator x) = Just x
        fromOperator _            = Nothing

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

-- added this for convenience in the CSharpGram file. 
sComma :: Parser Token Token
sComma =  symbol Comma

pFromMaybe :: (s -> Maybe a) -> Parser s a
pFromMaybe f = fromJust . f <$> satisfy (isJust . f)
