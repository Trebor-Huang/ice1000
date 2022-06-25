{-# LANGUAGE PolyKinds #-}

module UTLC where

import           Control.Exception
import           Data.Function
import           Data.Maybe
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T

type Ctor = Text

type a :@ b = a

data Strategy = Eager | Lazy

strategy :: a -> a -> Strategy -> a
strategy eager _lazy Eager = eager
strategy _eager lazy Lazy  = lazy

newtype UTLCException = UTLCThrow UTLC deriving (Show)

instance Exception UTLCException

data UTLC where
  ULam  :: (UTLC -> UTLC) -> UTLC
  UApp  :: UTLC -> UTLC -> UTLC
  UCon  :: Ctor -> [UTLC] -> UTLC
  UCase :: Strategy -> UTLC -> [(Maybe Ctor, [UTLC] -> Maybe UTLC)] -> UTLC

  UGet   :: (UTLC :@ "Input" -> UTLC) -> UTLC
  UPut   :: UTLC :@ "Output" -> UTLC -> UTLC
  UThrow :: UTLC :@ "Error" -> (UTLC -> UTLC) -> UTLC
  UCatch :: (UTLC -> UTLC) :@ "Handler" -> UTLC -> UTLC

instance Show UTLC where
  show (UCon ctor us) = "(" ++ unwords (T.unpack ctor : fmap show us) ++ ")"
  show _              = error "can't show a non-NF term"

eval :: Strategy -> UTLC -> IO UTLC
eval s = fix $ \rec utlc -> case utlc of
  UApp l x -> do
      l' <- rec l
      case l' of
        ULam f -> do
          x' <- strategy rec pure s x
          rec (f x')
        _      -> fail "non-function applied to an argument"
  UCase s' x cases -> do
      let recMaybe = maybe (fail "pattern has wrong arity") rec
      (evalX, x') <- case s' of
        Eager -> do
          x' <- eval Eager x
          pure (pure x', x')
        Lazy -> pure (eval Lazy x, x)
      case cases of
        [(Nothing, body)] -> recMaybe (body [x'])
        [] -> do
          evalX
          fail "empty case block reached"
        _ -> do
          x'' <- evalX
          let con = case x'' of
                UCon nm us -> Just (nm, us)
                _          -> Nothing
          mu <- case filter (\(p, _) -> isNothing p || p == fmap fst con) cases of
            ((Nothing, body) : _) -> pure (body [x''])
            ((Just _ , body) : _) -> let (_, us) = fromJust con in pure (body us)
            []                    -> fail "pattern match is non-exaustive"
          recMaybe mu
  UCon ctor us | Eager <- s -> do
      us' <- traverse rec us
      pure (UCon ctor us')
  UGet f -> do
      ctor <- T.getLine
      rec (f (UCon ctor []))
  UPut out u -> do
      out' <- strategy rec nf s out
      print out'
      rec u
  UThrow e _ -> throwIO (UTLCThrow e)
  UCatch handler u -> catch (rec u)
      (\(UTLCThrow e) -> rec (handler e))
  u -> pure u

nf :: UTLC -> IO UTLC
nf (UCon ctor us) = do
    us' <- traverse nf us
    pure $ UCon ctor us'
nf u = eval Lazy u
