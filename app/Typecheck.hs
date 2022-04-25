{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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

data TypeF scope term
  = TCon !Status !Name ![term]  -- Currently just this.
  deriving (Eq, Show)
$(deriveBifunctor ''TypeF)
$(deriveBifoldable ''TypeF)
$(deriveBitraversable ''TypeF)
type Type = FS TypeF

data FullType var
  = FTProgram
  | FTType (Type var)
  | FTHole (Type var)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- There is a major deficiency of the unification procedure:
-- It cannot deal with non-homogeneous types.
-- But probably it is out of the Haskell type system's reach.

data UTypeF scope term
  = UProgram
  | UHole !term
  | UType !term
  | UCon !Status !Name ![term]
  deriving (Eq, Show)
$(deriveBifunctor ''UTypeF)
$(deriveBifoldable ''UTypeF)
$(deriveBitraversable ''UTypeF)
type UType = FS UTypeF

embedT :: Type var -> UType var
embedT (Var var) = Var var
embedT (Con (TCon status s tms))
  = Con$UCon status
      s (map embedT tms)

embedFT :: FullType var -> UType var
embedFT FTProgram = Con UProgram
embedFT (FTType t) = embedT t
embedFT (FTHole t) = Con$UHole $ embedT t

reifyT :: UType var -> Type var
reifyT (Var var) = Var var
reifyT (Con (UCon status s tms)) = Con$TCon status s (map reifyT tms)
reifyT (Con _) = error "Unexpected type"

reifyUT :: UType var -> FullType var
reifyUT (Con UProgram) = FTProgram
reifyUT (Con (UHole t)) = FTHole $ reifyT t
reifyUT (Con (UType t)) = FTType $ reifyT t
reifyUT _ = error "Unexpected type"


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
  | Annotation info term (FullType Name)
    -- ^ Type annotation
  deriving (Show, Eq)
$(deriveBifunctor ''Ice100F)
$(deriveBifoldable ''Ice100F)
$(deriveBitraversable ''Ice100F)

type Ice100 info = FS (Ice100F info)

-- Challenge: write a correct type signature.

{-
infer :: UnifyEnv m HVar UTypeF
  => HVar -> Ice100 info HVar -> m (UType HVar)
infer h (Var v) = return $ Var v
infer h (Con (Call _ _ name args))
  = _wn

infer h (Con (Eff _ name args cont)) = _wq
infer h (Con (Case _ Nothing clauses)) = _wt
infer h (Con (Case _ (Just tm) clauses)) = _wu
infer h (Con (Prog _ tm1 tm2)) = do
  ty1 <- infer (h :-: ("InferProgLeft",0)) tm1
  ty2 <- infer (h :-: ("InferProgRight",0)) tm2
  -- unifyEqs True []
  _

infer h (Con (Annotation _ tm ty)) = do
  ty' <- infer (h :-: ("InferAnnotation",0)) tm
  unifyEqs False [(ty', rename ((h :-:) . (,0)) (embedFT ty))]
  export ty'

infer h (Con (Atom _ t)) = _
-}
