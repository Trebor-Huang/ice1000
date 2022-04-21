{-# LANGUAGE RankNTypes, QuantifiedConstraints, UndecidableInstances #-} -- Used for Show instances
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Scope (BVar, Scope, FS(..), substitute, subst, rename, clearVar) where
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable, deriveBitraversable)

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

class Bitraversable t => Unifiable t where
  zipMatch :: t a b -> t c d -> Maybe (t (a,c) (b,d))

instance Unifiable t => Unifiable (Solidify t) where
  zipMatch (EmbedF s) (EmbedF t) = EmbedF <$> zipMatch s t
  zipMatch (SolidF s) (SolidF t)
    | s == t = Just $ SolidF s
    | otherwise = Nothing
  zipMatch _ _ = Nothing

data UnifyError t var
  = Conflict (FS t var) (FS t var)
  | Occurs var (FS t var)
  | BoundLeak

class (Monad m, Unifiable t) => UnifyEnv m var t where
  giveUp :: UnifyError t var -> m a
  getVar :: var -> m (FS t var)
  setVar :: var -> FS t var -> m ()

unify :: UnifyEnv m v t => (FS t v, FS t v) -> m ()
unify (Var x, t) = setVar x t
unify (t, Var x) = setVar x t
unify ~(Con s, Con t) = case zipMatch s t of
  Nothing -> giveUp $ Conflict (Con s) (Con t)
  Just st -> do
    bitraverse (unify . bimap solidify solidify) unify st
    return ()

instance UnifyEnv m var t => UnifyEnv m var (Solidify t) where
  giveUp (Conflict s t) = case (clean (liquify s), clean (liquify t)) of
    (Just s', Just t') -> giveUp $ Conflict s' t'
    _ -> giveUp (BoundLeak :: UnifyError t var)
  giveUp (Occurs v t) = case clean (liquify t) of
    Nothing -> giveUp (BoundLeak :: UnifyError t var)
    Just t -> giveUp (Occurs v t)
  giveUp BoundLeak = giveUp (BoundLeak :: UnifyError t var)

  getVar v = do
    t <- getVar v :: m (FS t var)
    return $ solidify $ fmap Right t

  setVar v t = case clean (liquify t) of
    Nothing -> giveUp (BoundLeak :: UnifyError t var)
    Just t' -> setVar v t'

