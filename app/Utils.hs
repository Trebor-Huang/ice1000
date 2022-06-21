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
