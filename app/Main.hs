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

program100 :: Ice100 HVar
program100 = Con$Typecheck.Case Nothing
    [ (PCon "func" [PVar {- 0 : inA -}, PVar {- 1 : outC -}],
        Con$Typecheck.Prog
            (Var $ Right (HVRoot "f"))
            (Con$Call Constructor "func"
                [ Var (Left 0)
                , Con$Typecheck.Case Nothing
                    [ (PVar {- inB -}, Con$Typecheck.Prog
                        (Var $ Right (Right (HVRoot "g")))
                        (Con$Call Constructor "func"
                            [ Var (Left 0)
                            , Var (Right (Left 1))
                            ]))
                    ]
                ]))
    ]

env100 :: [Char] -> [FullType Name]
env100 "add" = [tyint, tyint, tyint]
    where
        tyint :: FullType Name
        tyint = FTByConstructor (Con$TCon "Int" [])
env100 "func" =  -- CoFunc a b
    [ FTByConstructor (Con$TCon "CoFunc" [Var "a", Var "b"])
    , FTByConstructor (Var "a")
    , FTByPattern (Var "b")
    ]
env100 "pair" =
    [ FTByConstructor (Con$TCon "Pair" [Var "a", Var "b"])
    , FTByConstructor (Var "a")
    , FTByConstructor (Var "b")
    ]
env100 "copair" =  -- Yes, they look the same. We need to check polarity.
    [ FTByConstructor (Con$TCon "CoPair" [Var "a", Var "b"])
    , FTByConstructor (Var "a")
    , FTByConstructor (Var "b")
    ]
env100 "unit" =
    [ FTByConstructor (Con$TCon "Unit" []) ]
env100 _ = error "Unknown name."

test100 :: Either UnifyError (Map.Map HVar (UType HVar))
test100 = runMapUnifyEnv $ infer env100 (HVRoot "") program100

main :: IO ()
main = print test100
