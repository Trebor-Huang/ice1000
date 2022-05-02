module Main where
import Scope
import Core
import Typecheck
import Data.Void (Void)
import qualified Data.Map as Map

program10 :: Ice10 a
program10 = Con $ Core.Eff "input" [] $
    Con $ Core.Eff "input" [] $
    Con $ Core.Eff "output" [Con $ Fun "+" [Var (Left 0), Var (Right (Left 0))]] $
    Con $ Core.Eff "abort" [] $
    Con $ Core.Case Eager (Just $ Var $ Left 0) [] Nothing

test10 :: IO (Either String (Ice10 Void))
test10 = runEnv Map.empty $ clearVar <$> eval Eager program10

program100 :: Ice100 () a
program100 = Con$Call () Function "add"
    [ Con$Typecheck.Atom () (AInt 3)
    , Con$Typecheck.Atom () (AInt 4)
    ]

test100 :: Either UnifyError (Map.Map HVar (UType HVar))
test100 = runMapUnifyEnv $ infer (\case
    "add" -> [tyint, tyint, tyint]
    _ -> error "Unknown name") (HVRoot "") program100
    where
        tyint :: FullType Name
        tyint = FTByConstructor (Con$TCon "Int" [])

main :: IO ()
main = print test100
