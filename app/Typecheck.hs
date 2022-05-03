{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Typecheck where
import Scope
import Core (Atomic(..), Name)
import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)

-- Tasks: check and infer the types
-- translating deep pattern matching to shallow casejumps
-- the order of @Prog@ be switched appropriately ?? (Bonus)

data TypeF scope term
  = TCon !Name ![term]  -- Currently just this.
  deriving (Eq, Show)
deriveBifunctor ''TypeF
deriveBifoldable ''TypeF
deriveBitraversable ''TypeF
type Type = FS TypeF

data FullType var
  = FTProgram
  | FTByConstructor (Type var)
  | FTByPattern (Type var)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- There is a major deficiency of the unification procedure:
-- It cannot deal with non-homogeneous types.
-- But probably it is out of the Haskell type system's reach.

data UTypeF scope term
  = UProgram
  | UByPattern !term
  | UByConstructor !term
  | UCon !Name ![term]
  deriving (Eq, Show)
$(deriveBifunctor ''UTypeF)
$(deriveBifoldable ''UTypeF)
$(deriveBitraversable ''UTypeF)
type UType = FS UTypeF

instance Unifiable UTypeF where
  zipMatch UProgram UProgram = Just UProgram
  zipMatch (UByPattern b) (UByPattern b') = Just $ UByPattern (b, b')
  zipMatch (UByConstructor b) (UByConstructor b') = Just $ UByConstructor (b, b')
  zipMatch (UCon s bs) (UCon s' bs')
    | s == s' = UCon s <$> (bs #? bs')
    | otherwise = Nothing
  zipMatch _ _ = Nothing

embedT :: Type var -> UType var
embedT (Var var) = Var var
embedT (Con (TCon s tms))
  = Con$UCon
      s (map embedT tms)

embedFT :: FullType var -> UType var
embedFT FTProgram = Con UProgram
embedFT (FTByConstructor t) = Con$UByConstructor $ embedT t
embedFT (FTByPattern t) = Con$UByPattern $ embedT t

reifyT :: UType var -> Type var
reifyT (Var var) = Var var
reifyT (Con (UCon s tms)) = Con$TCon s (map reifyT tms)
reifyT (Con _) = error "Unexpected type"

reifyUT :: UType var -> FullType var
reifyUT (Con UProgram) = FTProgram
reifyUT (Con (UByPattern t)) = FTByPattern $ reifyT t
reifyUT (Con (UByConstructor t)) = FTByConstructor $ reifyT t
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

instantiate :: HVar -> FullType Name -> UType HVar
instantiate hv ty = rename ((hv :-:) . (,0)) (embedFT ty)

fromPattern :: Pattern -> Ice100 () BVar
fromPattern p = snd $ helper 0 p
  where
    helper b PVar = (b+1, Var b)
    helper b (PCon n ps) = let (b', vs) = aux b ps in
      (b', Con$Call () Constructor n vs)
    
    aux b [] = (b, [])
    aux b (p:ps) =
      let (b' , v ) = helper b p
          (b'', vs) = aux b' ps in
      (b'', v:vs)

infer :: (UnifyEnv m HVar UTypeF, HasIO m)
  => (Name -> [FullType Name])  -- ^ Constant types
  -> HVar  -- ^ Current variable hierarchy
  -> Ice100 info HVar -> m (UType HVar)
infer env hv (Var v) = return (Var v)
infer env hv (Con (Call _ _ name args)) = do
  -- We've already done scope checking, so just do it.
  let (targetTy : argTys) = map (instantiate hv) $ env name
  argInfTys <- sequence [ infer env (hv :-: ("InferCallArgs", i)) a | (i,a) <- zip [0..] args ]
  unifyEqs False $ zip argInfTys argTys
  export targetTy

infer env hv (Con (Eff _ name args cont)) = do
  let (targetTy : argTys) = map (instantiate hv) $ env name
  argInfTys <- sequence [ infer env (hv :-: ("InferEffArgs", i)) a | (i,a) <- zip [0..] args ]
  unifyEqs False $ zip argInfTys argTys
  return $ Con UProgram

infer env hv (Con (Case _ Nothing clauses)) = do
  tyclauses <- mapM inferClauses (zip [0..] clauses)
  unifyEqs True $ map (Var hv,) tyclauses
  export $ Con$UByPattern $ Var hv
  where
    inferClauses :: (UnifyEnv m HVar UTypeF, HasIO m)
      => (Int, (Pattern, Ice100 info (Either BVar HVar)))
      -> m (UType HVar)
    inferClauses (i, (pat, clause)) = do
      typat <- infer env (hv :-: ("CheckPattern",0)) $
        rename ((hv:-:) . ("ClauseBoundVar",)) $
        fromPattern pat
      ty <- infer env (hv :-: ("InferClause", i)) $
        rename (\case
          Left b -> hv :-: ("ClauseBoundVar", b)
          Right hv -> hv) clause
      unifyEqs True [(ty, Con UProgram), (typat, Con$UByConstructor (Var hv))]
      export $ Var hv

infer env hv (Con (Prog _ tm1 tm2)) = do
  ty1 <- infer env (hv :-: ("InferProgLeft",0)) tm1
  ty2 <- infer env (hv :-: ("InferProgRight",0)) tm2
  unifyEqs True [(ty1, Con$UByPattern (Var hv)), (ty2, Con$UByConstructor (Var hv))]
  return $ Con UProgram

-- I'm lazy
infer env hv (Con (Case i (Just tm) clauses))
  = infer env hv (Con$Prog undefined (Con$Case undefined Nothing clauses) tm)

infer env hv (Con (Annotation _ tm ty)) = do
  ty' <- infer env (hv :-: ("InferAnnotation",0)) tm
  unifyEqs False [(ty', instantiate hv ty)]
  export ty'

infer env hv (Con (Atom _ (AInt _))) = return $ Con$UByConstructor $ Con$UCon "Int" []
infer env hv (Con (Atom _ (ABool _))) = return $ Con$UByConstructor $ Con$UCon "Bool" []
infer env hv (Con (Atom _ (AStr _))) = return $ Con$UByConstructor $ Con$UCon "Str" []
