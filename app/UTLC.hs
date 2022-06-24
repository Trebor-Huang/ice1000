{-# LANGUAGE PolyKinds #-}

module UTLC where

import           Control.Exception
import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T

type Ctor = Text

type a :@ b = a

data Strategy = Eager | Lazy

newtype UTLCException = UTLCThrow UTLC deriving (Show)

instance Exception UTLCException

data UTLC where
  ULam :: (UTLC -> UTLC) -> UTLC
  UApp :: UTLC -> UTLC -> UTLC
  UCon :: Ctor -> [UTLC] -> UTLC
  UCase :: Strategy -> UTLC -> [(Maybe Ctor, [UTLC] -> Maybe UTLC)] -> UTLC

  UGet :: (UTLC :@ "Input" -> UTLC) -> UTLC
  UPut :: UTLC :@ "Output" -> UTLC -> UTLC
  UThrow :: UTLC :@ "Error" -> (UTLC -> UTLC) -> UTLC
  UCatch :: (UTLC -> UTLC) :@ "Handler" -> UTLC -> UTLC

instance Show UTLC where
  show (UCon ctor us) = "(" ++ T.unpack ctor ++ " " ++ unwords (fmap show us) ++ ")"
  show _ = error "can't show a non-NF term"

app :: (UTLC -> IO UTLC) -> Strategy -> UTLC -> UTLC -> IO UTLC
app eval strategy l x = do
  l' <- eval l
  x' <- case strategy of
    Eager -> eval x
    Lazy  -> pure x
  case l' of
    ULam f -> eval $ f x'
    _      -> fail "non-function applied to an argument"

casing :: (UTLC -> IO UTLC) -> Strategy -> UTLC -> [(Maybe Ctor, [UTLC] -> Maybe UTLC)] -> IO UTLC
casing eval strategy x cases = do
  let evalMaybe mu = case mu of
        Just u  -> eval u
        Nothing -> fail "pattern has wrong arity"
  (evalX, x') <- case strategy of
    Eager -> do
      x' <- eager x
      pure (pure x', x')
    Lazy -> pure (whnf x, x)
  case cases of
    [] -> fail "empty case block reached"
    [(Nothing, body)] -> evalMaybe (body [x'])
    _ -> do
      x'' <- evalX
      let con = case x'' of
            UCon nm us -> Just (nm, us)
            _          -> Nothing
      mu <- case filter (\(p, _) -> isNothing p || p == fmap fst con) cases of
        ((Nothing, body) : _) -> pure $ body [x'']
        ((Just _, body) : _)  -> let (_, us) = fromJust con in pure $ body us
        []                    -> fail "pattern match is non-exaustive"
      evalMaybe mu

eff :: (UTLC -> IO UTLC) -> UTLC -> IO UTLC
eff eval x = case x of
  UGet f -> do
    s <- T.getLine
    eval (f (UCon s []))
  UPut s u -> do
    s' <- eager s
    print s'
    eval u
  UThrow e _ -> throwIO (UTLCThrow e)
  UCatch handler u -> do
    catch
      (eval u)
      (\(UTLCThrow e) -> eval (handler e))
  _ -> fail "'eff' only handles effects"

whnf :: UTLC -> IO UTLC
whnf (ULam f)          = pure (ULam f)
whnf (UApp l x)        = app whnf Lazy l x
whnf (UCase s x cases) = casing whnf s x cases
whnf (UCon ctor us)    = pure (UCon ctor us)
whnf u                 = eff whnf u

nf :: UTLC -> IO UTLC
nf (UCon ctor us) = do
  us' <- traverse nf us
  pure $ UCon ctor us'
nf u = whnf u

eager :: UTLC -> IO UTLC
eager (ULam f) = pure (ULam f)
eager (UApp l x) = app eager Eager l x
eager (UCase s x cases) = casing eager s x cases
eager (UCon ctor us) = do
  us' <- traverse eager us
  pure $ UCon ctor us'
eager u = eff eager u
