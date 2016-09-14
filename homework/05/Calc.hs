{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Mul x y) = (eval x) * (eval y)
eval (ExprT.Add x y) = (eval x) + (eval y)

parseStr :: String -> Maybe Integer
parseStr = fmap (eval) . parseExp ExprT.Lit ExprT.Add ExprT.Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance (Expr Integer) where
    lit = id
    mul = (*)
    add = (+)

instance (Expr Bool) where
    lit x = if x <= 0 then False else True
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance (Expr MinMax) where
    lit = MinMax
    mul (MinMax x) (MinMax y) = MinMax (max x y)
    add (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance (Expr Mod7) where
    lit x = Mod7 (mod x 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

instance (Expr StackVM.Program) where
    lit x = [StackVM.PushI x]
    mul x y = x ++ y ++ [StackVM.Mul]
    add x y = x ++ y ++ [StackVM.Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul
