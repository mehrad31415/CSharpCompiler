-- Mehrad Haghshenas (2822865)

module Main where

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode
import Prelude hiding ((<$), (<*), (*>))


main :: IO ()
main = do
    -- get command line arguments
    args  <- getArgs
    files <- case args of
              []  ->  do
                putStrLn "no argument given; assuming example.cs"
                return ["example.cs"]
              xs  ->  return xs
    -- translate each of the files
    processFiles files

-- given a list of source filepaths, produce an ssm output file for each of them.
processFiles :: [FilePath] -> IO ()
processFiles = mapM_ $ processFile
        . \f -> (f, addExtension (dropExtension f) "ssm")


-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = formatCode
                . foldCSharp codeAlgebra
                . run "parser" (pClass <* eof)
                . run "lexer" lexicalScanner

run :: (Show s, Show a) => String -> Parser s a -> [s] -> a
run s p x = fst . headOrError . filter (null . snd) . parse p $ x
  where
    headOrError (x:xs) = x
    headOrError [] = error $ 
      "The " <> s <> " returned no full parses. Here are all parses that didn't consume the entire input:\n" 
      <> show (parse p x)
