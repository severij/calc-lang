module Main where

import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Except (forever, runExcept)
import System.IO (hFlush, stdout)
import Parser (expr)
import Expr (eval)

main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout
    l <- getLine
    putStrLn $ case parse expr "input" l of
        Left err  -> errorBundlePretty err
        Right exp -> case runExcept $ eval exp of
            Left e  -> e
            Right n -> either show show n
