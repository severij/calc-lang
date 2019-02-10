module Expr where

import Data.Either
import Data.Bifunctor
import Control.Monad.Except


data Expr = Cons (Either Double Integer)
          | Var String
          | Neg Expr
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Let String Expr Expr
          deriving (Show)


replace :: String -> Expr -> Expr -> Expr 
replace _ _ (Cons c)   = Cons c
replace s1 e1 (Var s2) = if s1 == s2 then e1 else Var s2
replace s1 e1 (Neg e2) = Neg $ replace s1 e1 e2
replace s1 e1 (e2 :+: e3) = replace s1 e1 e2 :+: replace s1 e1 e3
replace s1 e1 (e2 :-: e3) = replace s1 e1 e2 :-: replace s1 e1 e3
replace s1 e1 (e2 :*: e3) = replace s1 e1 e2 :*: replace s1 e1 e3
replace s1 e1 (e2 :/: e3) = replace s1 e1 e2 :/: replace s1 e1 e3
replace s1 e1 (Let v e2 e3) = Let v (replace s1 e1 e2) (replace s1 e1 e3)


op :: (Num a, Num b, Show a, Show b)
    =>(a -> a -> a) 
   -> (b -> b -> b)
   -> Either a b
   -> Either a b
   -> Except String (Either a b)
op o1 o2 (Left n1)  (Left n2) = return $ Left (o1 n1 n2)
op o1 o2 (Right n1) (Right n2) = return $ Right (o2 n1 n2)
op _ _   (Left n1)  (Right n2) =
    throwError $ "Error: Unmatching types (" ++ show n1 ++ " :: Double, " ++
               show n2 ++ " :: Integer)"
op _ _   (Right n1)  (Left n2) =
    throwError $ "Error: Unmatching types (" ++ show n1 ++ " :: Integer, " ++
               show n2 ++ " :: Double)"


eval :: Expr -> Except String (Either Double Integer)
eval (Cons c) = return $ c
eval (Var v)  = throwError $ "Error: Undefined variable: " ++ v
eval (Neg e)  = eval e >>= return . bimap negate negate
eval (e1 :+: e2) = eval e1 >>= \n1 -> eval e2 >>= \n2 -> op (+) (+) n1 n2
eval (e1 :-: e2) = eval e1 >>= \n1 -> eval e2 >>= \n2 -> op (-) (-) n1 n2
eval (e1 :*: e2) = eval e1 >>= \n1 -> eval e2 >>= \n2 -> op (*) (*) n1 n2
eval (e1 :/: e2) = eval e1 >>= \n1 -> eval e2 >>= \n2 -> op (/) (div) n1 n2
eval (Let v e1 e2) = eval $ replace v e1 e2

