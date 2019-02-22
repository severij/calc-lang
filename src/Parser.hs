module Parser where

import Data.Void (Void)
import Data.Char (isSpace)

import Text.Megaparsec (Parsec, (<|>))
import Control.Monad.Combinators (between, eitherP, many)
import Control.Monad.Combinators.Expr (makeExprParser, Operator( InfixL, Prefix ))
import Text.Megaparsec (takeWhile1P, try)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Expr

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

cons :: Parser (Either Double Integer)
cons = lexeme $ eitherP (try L.float) L.decimal

let' :: Parser String
let' = lexeme $ string "let"

in' :: Parser String
in' = lexeme $ string "in"

equals :: Parser Char
equals = lexeme $ char '='

plus :: Parser Char
plus = lexeme $ char '+'

minus :: Parser Char
minus = lexeme $ char '-'

asterisk :: Parser Char
asterisk = lexeme $ char '*'

slash :: Parser Char
slash = lexeme $ char '/'

leftParen :: Parser Char
leftParen = lexeme $ char '('

rightParen :: Parser Char
rightParen = lexeme $ char ')'

underscore :: Parser Char
underscore = lexeme $ char '_'

var :: Parser String
var = lexeme $ do first <- letterChar <|> underscore
                  rest  <- many $ alphaNumChar <|> underscore
                  return $ first : rest


operators :: [[Operator Parser Expr]]
operators = [ [ Prefix (Neg <$ char '-') ]
            , [ InfixL ((:*:) <$ asterisk)
              , InfixL ((:/:) <$ slash)
              ]
            , [ InfixL ((:+:) <$ plus)
              , InfixL ((:-:) <$ minus)
              ]
            ]

parens :: Parser a -> Parser a
parens = between leftParen rightParen

term :: Parser Expr
term = parens expr <|> Cons <$> cons <|> Var <$> var

expr' :: Parser Expr
expr' = makeExprParser term operators

expr :: Parser Expr
expr = parens expr
   <|> do let' 
          v  <- var
          equals
          e1 <- expr
          in'
          e2 <- expr
          return $ Let v e1 e2
   <|> expr'
   <|> term
