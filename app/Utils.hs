{-# LANGUAGE FlexibleInstances #-}
module Utils where

-- | Atomic types used in the program.
data Atomic
  = AInt !Int
  | ABool !Bool
  | AStr !String
  deriving (Show, Eq)

type Name = String

data Scoped a = Scoped
  { names :: ![Name]
  , unscope :: !a }
  deriving (Show, Eq)

class Subs a b where
  subs :: (Name -> Maybe a) -> b -> b

instance Subs a b => Subs a (Scoped b) where
  subs s (Scoped ns t) = Scoped ns
    (subs (\n -> if n `elem` ns then Nothing else s n) t)
