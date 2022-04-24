{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
module Core where
import Scope
import Data.Bifunctor.TH
    ( deriveBifoldable, deriveBifunctor, deriveBitraversable )
import Data.Void (Void)
import Data.Bitraversable (Bitraversable(bitraverse))
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map

type Name = String  -- Change later
data Eagerness = Eager | Lazy deriving (Eq, Show, Ord, Enum)
data Normal = WNF | WHNF | RF deriving (Eq, Show, Ord, Enum)

data Atomic = AInt !Int | ABool !Bool | AStr !String deriving Eq
instance Show Atomic where
  show (AInt a) = show a
  show (ABool a) = show a
  show (AStr a) = show a

-- | The core language
data Ice10F scope term
  = Cons !Name ![term] -- ^ Constructors
  | Cut !Name ![term] -- ^ Functions, fully applied
  | Eff !Name ![term] !scope
    -- ^ Builtin Effects
  | Fun !Name ![term]
    -- ^ Builtin Functions
  | Atom !Atomic
    -- ^ Basic datatypes
  | Case !Eagerness !(Maybe term) ![(Name, scope)]
    -- ^ Case distinctions either on the currently focused continuation
    -- or on a term, @(Name, scope)@ gives a constructor label, and binds
    -- some pattern variables.
  | Prog !term !term
    -- ^ The former @term@ must be defined by patterns, and the latter defined
    -- by constructors.
  deriving (Show, Eq)
$(deriveBifunctor ''Ice10F)
$(deriveBifoldable ''Ice10F)
$(deriveBitraversable ''Ice10F)

type Ice10 = FS Ice10F

-- We now define the operational semantics for the core language.
class MonadFail m => Ice10Eff m where
  handle
    :: Name  -- ^ Effect name
    -> [Ice10 Void]  -- ^ Arguments
    -> Scope Ice10 v  -- ^ Continuation Term
    -> (Ice10 v -> m (Ice10 v))  -- ^ Inverted control
    -> m (Ice10 v)

class Ice10Eff m => Ice10Env m e | m -> e where
  push :: [Ice10 (e, BVar)] -> m e
    -- ^ Pushes in values for some bound variables,
    -- returns a position token used to fetch back the values.
  fetch :: (e, BVar) -> m (Normal, Ice10 (e, BVar))
  write :: (e, BVar) -> Normal -> Ice10 (e, BVar) -> m ()
  getConst :: Name -> m (Ice10 BVar)
    -- Since we've done scope checking this is fine.
  evalFun :: Name -> [Ice10 Void] -> m (Ice10 Void)

eval :: Ice10Env m e => Eagerness -> Ice10 (e, BVar) -> m (Ice10 (e, BVar))
eval b (Var m) = do
  -- For variables, we fetch the variable, evaluate and cache it.
  (n, expr) <- fetch m
  let n' = toEnum (fromEnum b)  -- !
  expr' <- if n > n'
    then eval b expr
    else return expr
  write m n' expr'
  return expr'
eval Eager (Con (Cons c args)) = do
  args <- mapM (eval Eager) args
  return (Con (Cons c args))
eval Lazy c@(Con (Cons _ _)) = return c
eval b (Con (Cut f args)) = do
  e <- push args
  body <- getConst f
  eval b (rename (e,) body)
eval b (Con (Fun f args)) = do
  args' <- mapM (eval Eager) args
  rename undefined <$> evalFun f (map clearVar args')
eval _ a@(Con (Atom _)) = return a
eval b (Con (Eff eff args cont)) = do
  args' <- mapM (eval Eager) args
  handle eff (map clearVar args') cont (eval b)
eval b c@(Con (Case b' Nothing _)) = return c
eval b (Con (Case b' (Just t) clauses)) = do
  ~(Con (Cons c args)) <- eval b' t
  case lookup c clauses of
    Nothing -> fail "Pattern matching not exhaustive."
    Just clause -> do
      e <- push args
      eval b (rename (either (e,) id) clause)
eval b (Con (Prog pat con)) = do
  ~(Con (Case b' Nothing clauses)) <- eval Lazy pat
  eval b (Con (Case b' (Just con) clauses))

-- | Chase down all the unsubstituted variables, bad exercise.
-- complete :: Ice10Env m e => Ice10 (e, BVar) -> m (Ice10 Void)
-- complete (Var e) = complete =<< fetch e
-- complete (Con t) = Con <$> bitraverse
--     (fmap join . traverse
--       (fmap sequenceA . traverse
--         (complete <=< fetch)))
--     complete t

newtype Env a = Env {fromEnv ::
  ReaderT (Map.Map Name (Ice10 BVar)) (
  StateT (Int, Map.Map (Int, BVar) (Normal, Ice10 (Int, BVar))) (
  ExceptT String
  IO)) a} deriving (Functor, Applicative, Monad, MonadFail)
runEnv :: Map.Map Name (Ice10 BVar) -> Env a -> IO (Either String a)
runEnv c
  = runExceptT
  . flip evalStateT (0, Map.empty)
  . flip runReaderT c
  . fromEnv

instance Ice10Eff Env where
  handle "abort" [] cont return = return $ Con $ Cons "" []
  handle "input" [] cont return = do
    result <- Env $ lift $ lift $ lift (readLn :: IO Int)
    return $ substitute [Con $ Atom (AInt result)] cont
  handle "output" [arg] cont return = do
    Env $ lift $ lift $ lift $ print arg
    return $ substitute [] cont
  handle eff args cont return = fail $ "Unsupported effect: " ++ eff

instance Ice10Env Env Int where
  push ts = do
    (e, keys) <- Env (lift get)
    let e' = e + 1
    let keys' = keys `Map.union` Map.fromDistinctAscList
          [((e', i), (RF, r)) | (i,r) <- zip [0..] ts]
    return e'
  fetch k = do
    (_, keys) <- Env (lift get)
    return (keys Map.! k)
  write k n t = do
    (e, keys) <- Env (lift get)
    Env $ lift $ put (e, Map.insert k (n,t) keys)
  getConst c = do
    Env $ asks (Map.! c)
  evalFun "+" [Con (Atom (AInt m)), Con (Atom (AInt n))] = do
    return $ Con (Atom (AInt (m+n)))
  evalFun f _ = fail $ "Unrecognized built in: " ++ f
