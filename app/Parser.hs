module Parser where
import Utils

-- Ice1000
data Ice1000
  = Var Name
  | Con { label :: Name, args :: [Ice1000] }
  | Fun { label :: Name, args :: [Ice1000] }
  | Eff { label :: Name, args :: [Ice1000], cont :: Scoped Ice1000 }
  | Prog { bypat :: Ice1000, bycon :: Ice1000 }
  | Case { args :: [Ice1000], clauses :: [(Pattern, Ice1000)] }
  | Cont { clauses :: [(Pattern, Ice1000)] }
  | Atom { atom :: Atomic }
  deriving (Show, Eq)

data Pattern
  = PVar Name
  | PCon { plabel :: Name, pargs :: [Pattern] }
  deriving (Show, Eq)

data Type
  = TVar Name
  | TCon { tlabel :: Name, targs :: [Type] }
  deriving (Show, Eq)



