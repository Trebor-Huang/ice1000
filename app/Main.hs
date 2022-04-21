module Main where
import Scope
import Core
import Data.Void (Void)
import qualified Data.Map as Map

program :: Core a
program = Con $ Eff "input" [] $
    Con $ Eff "input" [] $
    Con $ Eff "output" [Con $ Fun "and" [Var (Left 0), Var (Right (Left 0))]] $
    Con $ Eff "abort" [] $
    Con $ Case Eager (Just $ Var $ Left 0) []

test :: IO (Either String (Core Void))
test = runEnv Map.empty $ clearVar <$> eval Eager program

main :: IO ()
main = do
    putStrLn "\n==== Test Start ===="
    res <- test
    putStrLn "\n====  Test End  ===="
    print res
