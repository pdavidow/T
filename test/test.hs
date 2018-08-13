import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Transformers
import qualified Data.Map as Map     

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 


exampleExp :: Exp
exampleExp = 
    Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))


unitTests = testGroup "Unit tests" $
    [ testGroup "exampleExp" $
        [ testCase "p3" $
            eval0 Map.empty exampleExp @?= IntVal 18
        ]
    ]