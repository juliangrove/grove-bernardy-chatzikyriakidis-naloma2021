{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Representations.Eval.Model where

newtype Entity = A { getInt :: Int } deriving (Show, Eq, Enum)

entities :: [Entity]
entities = [A 0..A 100]

-- | Properties

dog' :: Entity -> Bool
dog' x = x == A 1 || x == A 2

cat' :: Entity -> Bool
cat' x = x == A 3 || x == A 4

happy' :: Entity -> Bool
happy' x = x == A 1 || x == A 3

-- | Binary relations

chase' :: Entity -> Entity -> Bool
chase' x y = (y == A 1 && (x == A 3 || x == A 4)) || (y == A 2 && x == A 4)

catch' :: Entity -> Entity -> Bool
catch' x y = (y == A 1 && x == A 4) || (y == A 2 && x == A 4)
