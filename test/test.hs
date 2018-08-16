import Test.Tasty 
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Transformers
import Parents (Person(..), Gender(..), paternalGrandfather, paternalGrandfather1)
import qualified Data.Map as Map     
import Control.Monad
import Control.Monad.Trans


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 


exampleExp :: Exp
exampleExp = 
    Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))


unitTests = testGroup "Unit tests" $
    [ testGroup "Example Expression" $
        [ testCase "p3" $
            eval0 Map.empty exampleExp @?= IntVal 18
        ]
    , testGroup "Parents" $
        [ testCase "paternalGrandfather" $ do 
            gf <- paternalGrandfather $ Person Male
            if elem gf [Just $ Person Male, Nothing]
                then pure ()
                else assertFailure "paternalGrandfather" 
        , testCase "paternalGrandfather1" $ do 
            gf <- paternalGrandfather1 $ Person Male
            if elem gf [Just $ Person Male, Nothing]
                then pure ()
                else assertFailure "paternalGrandfather1"                 
        ]    
    ]