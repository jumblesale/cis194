{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Map.Strict as M
import Data.Char as C

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

scores = M.fromList $ zip ['A'..'Z'] [1,3,3,2,14,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

score :: Char -> Score
score x = case M.lookup (C.toUpper x) scores of
    Nothing  -> 0
    (Just y) -> y

scoreString :: String -> Score
scoreString x = sum (Prelude.map score x)
