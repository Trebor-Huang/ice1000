{-# LANGUAGE RankNTypes, QuantifiedConstraints, UndecidableInstances #-} -- Used for Show instances
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
module Scope where
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isJust)
import Control.Monad ((<=<))

-- | Safe zipping
(#?) :: [a] -> [b] -> Maybe [(a, b)]
[] #? [] = Just []
(x:xs) #? (y:ys) = ((x,y):) <$> xs #? ys
_ #? _ = Nothing

type BVar = Int
type Scope term var = term (Either BVar var)

data FS t a
  = Var !a
  | Con !(t (Scope (FS t) a) (FS t a))

substitute :: Monad term => [term a] -> Scope term a -> term a
substitute s t = t >>= \case
  Left n -> s !! n
  Right a -> return a

instance (Show a, forall p q. (Show p, Show q) => Show (t p q))
  => Show (FS t a) where
  show (Var a) = show a
  show (Con t) = show t

instance Bifunctor t => Functor (FS t) where
  fmap f (Var a) = Var (f a)
  fmap f (Con t) = Con (bimap (fmap (fmap f)) (fmap f) t)

instance Bifunctor t => Applicative (FS t) where
  pure = Var
  f <*> x = f >>= \f -> x >>= \x -> pure (f x)

instance Bifunctor t => Monad (FS t) where
  Var x >>= f = f x
  Con t >>= f = Con (bimap (>>= traverse f) (>>= f) t)

instance Bifoldable t => Foldable (FS t) where
  foldMap f (Var a) = f a
  foldMap f (Con t) = bifoldMap
    (foldMap $ either (const mempty) f)
    (foldMap f)
    t

instance Bitraversable t => Traversable (FS t) where
  traverse f (Var a) = Var <$> f a
  traverse f (Con t) = Con <$>
    bitraverse (traverse (traverse f)) (traverse f) t

subst :: Bifunctor t => (vars -> FS t vars') -> FS t vars -> FS t vars'
subst = (=<<)

rename :: Bifunctor t => (vars -> vars') -> FS t vars -> FS t vars'
rename = fmap

-- | Calculate the free variables
freeVar :: (Bifoldable t, Ord vars) => FS t vars -> Set.Set vars
freeVar = foldMap Set.singleton

-- | Assert that there are no unsubstituted variables.
clearVar :: Bifunctor t => FS t vars -> FS t void
clearVar = rename (error "IMPOSSIBLE: Dangling free variables.")

data Solidify t1 scope term
  = EmbedF !(t1 scope term)
  | SolidF !BVar
  deriving (Eq, Show)
$(deriveBifunctor ''Solidify)
$(deriveBifoldable ''Solidify)
$(deriveBitraversable ''Solidify)

solidify :: Bifunctor t => FS t (Either BVar b) -> FS (Solidify t) b
solidify (Var (Left a)) = Con$SolidF a
solidify (Var (Right b)) = Var b
solidify (Con t) = Con$EmbedF $ bimap (solidify . fmap helper) solidify t
  where
    helper :: Either a (Either b c) -> Either b (Either a c)
    helper (Left a) = Right (Left a)
    helper (Right (Left b)) = Left b
    helper (Right (Right c)) = Right (Right c)

liquify :: Bifunctor t => FS (Solidify t) b -> FS t (Either BVar b)
liquify (Var b) = Var$Right b
liquify (Con (SolidF a)) = Var$Left a
liquify (Con (EmbedF t)) = Con $ bimap liquify liquify t

clean :: Bitraversable t => FS t (Either a b) -> Maybe (FS t b)
clean = traverse $ \case
   Left _ -> Nothing
   Right b -> Just b

class Inject p q where
  inject :: p -> q
  reject :: q -> Maybe p
instance Inject p q => Inject p (Either r q) where
  inject = Right . inject
  reject = reject <=< either (const Nothing) Just
instance Inject p p where
  inject = id
  reject = Just

class Bitraversable t => Unifiable t where
  zipMatch :: t a b -> t c d -> Maybe (t (a,c) (b,d))

instance Unifiable t => Unifiable (Solidify t) where
  zipMatch (EmbedF s) (EmbedF t) = EmbedF <$> zipMatch s t
  zipMatch (SolidF s) (SolidF t)
    | s == t = Just $ SolidF s
    | otherwise = Nothing
  zipMatch _ _ = Nothing

data UnifyError
  = Conflict
  | Occurs
    -- ^ Note that the occurs check should be implemented by @UnifyEnv@.
  | BoundLeak
  -- deriving (Functor, Foldable, Traversable)

class (Monad m, Unifiable t) => UnifyEnv m var t where
  giveUp :: UnifyError -> m a
  getVar' :: var -> m (Maybe (FS t var))
  getVar :: var -> m (FS t var)
  getVar v = do
    t <- getVar' v
    case t of
      Nothing -> return $ Var v
      Just t -> return t
  setVar :: var -> FS t var -> m ()

unify :: forall v m w t. (UnifyEnv m v t, Inject v w)
  => (FS t w, FS t w) -> m ()
unify (Var v, t) = case (reject v, traverse reject t) of
  (Just v, Just t) -> do
    m <- getVar' v :: m (Maybe (FS t v))
    case m of
      Just t' -> unify @v (t', t)
      Nothing -> setVar v t
  _ -> giveUp @_ @v @t BoundLeak
unify (t, Var x) = unify @v (Var x, t)  -- I am Lazy
unify ~(Con s, Con t) = case zipMatch s t of
  Nothing -> giveUp @_ @v @t Conflict
  Just st -> do
    bitraverse (unify @v) (unify @v) st
    return ()

-- We now define a spartan unification environment
newtype MapUnifyEnv t var a = MapUnifyEnv
  {fromMapUnifyEnv ::
  StateT
    ( Set.Set var {- Free variables -}
    , Map.Map var (FS t var) {- Solved variables -}) (
  ExceptT UnifyError
  Identity) a} deriving (Functor, Applicative, Monad)

instance (Unifiable t, Ord var) => UnifyEnv (MapUnifyEnv t var) var t where
  giveUp e = MapUnifyEnv $ lift $ throwE e
  getVar' v = MapUnifyEnv $ do
    (_, m) <- get
    return $ m Map.!? v
  setVar v t = if v `Set.member` freeVar t then  -- Might be costly
      giveUp @_ @var @t Occurs
    else MapUnifyEnv $ do
      (vs, m) <- get
      let t' = subst (\v -> case m Map.!? v of
            Nothing -> Var v
            Just tm -> tm) t
      put (Set.delete v vs,
        subst (\w -> if v == w then t' else Var w) <$> m)
