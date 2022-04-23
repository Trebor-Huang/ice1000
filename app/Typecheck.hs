{-# LANGUAGE TemplateHaskell #-}
module Typecheck where
import Scope
import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)
import qualified Data.Map as Map

data TypeF scope term
  = Leaf term term
  | Bind scope
  deriving (Eq, Show)
$(deriveBifunctor ''TypeF)
$(deriveBifoldable ''TypeF)
$(deriveBitraversable ''TypeF)
type Type = FS TypeF

instance Unifiable TypeF where
  zipMatch (Leaf s t) (Leaf s' t') = Just (Leaf (s, s') (t, t'))
  zipMatch (Bind c) (Bind c') = Just (Bind (c, c'))
  zipMatch _ _ = Nothing

ta :: Type Int
ta = Con$Leaf (Con$Leaf (Var 0) (Con$Bind (Var (Left 0)))) (Var 2)

tb :: Type Int
tb = Con$Leaf (Var 2) (Con$Leaf (Var 1) (Con$Bind (Var (Left 0))))

unifTask :: MapUnifyEnv TypeF Int ()
unifTask = unifyEqs [(ta, tb)]

unifRes :: Either UnifyError (Map.Map Int (FS TypeF Int))
unifRes = runMapUnifyEnv unifTask
