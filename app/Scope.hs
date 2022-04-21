{-# LANGUAGE RankNTypes, QuantifiedConstraints, UndecidableInstances #-} -- Used for Show instances
module Scope (BVar, Scope, FS(..), substitute, subst, rename, clearVar) where
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Void (Void)

type BVar = Int
type Scope term var = term (Either BVar var)

data FS t a
  = Var !a
  | Con !(t (Scope (FS t) a) (FS t a))

data Fuse t1 t2 scope term
  = LeftF !(t1 scope term)
  | RightF !(t2 scope term)
  deriving (Eq, Show)

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
clearVar :: Bifunctor t => FS t vars -> FS t Void
clearVar = rename (error "IMPOSSIBLE: Dangling free variables.")
