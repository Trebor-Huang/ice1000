{-# LANGUAGE FlexibleContexts #-}
module Typecheck where
import Scope
import Core (Atomic(..), Name)
import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)

-- Tasks: check and infer the types
-- translating deep pattern matching to shallow casejumps
-- the order of @Prog@ be switched appropriately ?? (Bonus)

data Status
  = EagerPositive
  | EagerNegative
  | LazyPositive
  | LazyNegative
  deriving (Eq, Show, Enum, Bounded)

-- There is a major deficiency of the unification procedure:
-- It cannot deal with non-homogeneous types.
-- But probably it is out of the Haskell type system's reach.

data TypeF scope term
  = TCon !Name ![term]  -- Currently just this.
  deriving (Eq, Show)
$(deriveBifunctor ''TypeF)
$(deriveBifoldable ''TypeF)
$(deriveBitraversable ''TypeF)

instance Unifiable TypeF where
  zipMatch (TCon s bs) (TCon s' bs')
    | s == s' = TCon s <$> (bs #? bs')
    | otherwise = Nothing

type Type = FS TypeF

data Pattern
  = PVar
  | PCon !Name ![Pattern]
  deriving (Eq, Show)
data PatternVar
  = PVThis  -- ^ The variable here
  | PVThat !Int !PatternVar
    -- ^ The @Int@ points to the argument.
  deriving (Eq, Show)
-- | Flattens pattern variables to use indices.
-- This is suboptimal.
flattenPV :: Pattern -> PatternVar -> Int
flattenPV PVar PVThis = 0
flattenPV (PCon _ pats) (PVThat i pv) =
  sum (map countPV $ take i pats) + flattenPV (pats !! i) pv
flattenPV _ _ = error "Dangling pattern variable."

countPV :: Pattern -> Int
countPV PVar = 1
countPV (PCon _ pats) = sum $ map countPV pats

data CallType
  = Constructor
  | Function
  | BuiltinFunction
  deriving (Eq, Show, Enum, Bounded)

data Ice100F info scope term
  = Call info !CallType !Name ![term]
    -- ^ These three have similar syntax and typechecking details.
  | Case info !(Maybe term) ![(Pattern, scope)]
  | Prog info !term !term
  | Eff info !Name ![term] !scope
  | Atom info !Atomic
  | Annotation info term (Type Name)
    -- ^ Type annotation
  deriving (Show, Eq)
$(deriveBifunctor ''Ice100F)
$(deriveBifoldable ''Ice100F)
$(deriveBitraversable ''Ice100F)

type Ice100 info = FS (Ice100F info)

-- infer :: UnifyEnv m HVar TypeF
--   => HVar -> Ice100 info var -> m (Type HVar)
-- infer h (Var v) = return $ Ordinary (Var h)
-- infer h (Con (Call _ _ name args))
--   = _wn
-- infer h (Con (Eff _ name args cont)) = _wq
-- infer h (Con (Case _ Nothing clauses)) = _wt
-- infer h (Con (Case _ (Just tm) clauses)) = _wu
-- infer h (Con (Prog _ tm1 tm2)) = _wp
-- infer h (Con (Atom _ (AInt _)))
--   = return $ Ordinary $ Con$TCon "Int" []
-- infer h (Con (Atom _ (ABool _)))
--   = return $ Ordinary $ Con$TCon "Bool" []
-- infer h (Con (Atom _ (AStr _)))
--   = return $ Ordinary $ Con$TCon "Str" []
-- infer h (Con (Annotation _ tm ty)) = do
--   ty' <- infer (h :-: "InferAnnotation") tm
--   unifyEqs False [(_ty', rename (HVRoot "?global" :-:) ty)]
--   _
