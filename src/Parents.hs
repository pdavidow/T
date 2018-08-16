module Parents
    ( Person(..)
    , Gender(..)
    , lookupParent
    , lookupParent1
    , grandparent
    , grandparent1
    , greatGrandparent
    , greatGrandparent1
    , paternalGrandfather
    , paternalGrandfather1
    , maternalGrandfather
    , maternalGrandfather1
    , bothGrandfathers
    , bothGrandfathers1
    )

    where

-- https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
--      Every expression in a do block must be from the same monad.
--      To transform an IO expression into a MaybeT IO expression use liftIO.
--      Every monad transformer has a "run" function.
        -- The run function "runs" the top-most layer of a monad stack returning a value from the inside layer.
        -- For MaybeT IO, the run function is:
        -- runMaybeT :: MaybeT IO a -> IO (Maybe a)

-- https://hackage.haskell.org/package/MaybeT-0.1.2/docs/Control-Monad-Maybe.html


import System.Random    
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe  
 
data Person = Person Gender deriving (Eq, Show)
data Gender = Male | Female deriving (Eq, Show)


lookupOdds :: Gender -> Int
lookupOdds gender =
    case gender of
        Male   -> 3
        Female -> 5


randomPickWithinOdds :: Int -> IO Int
randomPickWithinOdds rangeMax = do 
    gen <- getStdGen   
    let (n, _) = randomR (1, rangeMax) gen
    pure n   
    

mLookupParent :: Maybe Person -> Gender -> IO (Maybe Person)         
mLookupParent mPerson gender = do
    case mPerson of 
        Just person -> 
            lookupParent person gender

        Nothing -> 
            pure Nothing


lookupParent :: Person -> Gender -> IO (Maybe Person) 
lookupParent _ gender = do 
    let odds = lookupOdds gender  
    randN <- randomPickWithinOdds odds  

    let mPerson =
            if randN == odds then
                Just $ Person gender 
            else
                Nothing

    pure mPerson
    

lookupParent1 :: Person -> Gender -> MaybeT IO Person
lookupParent1 _ gender = do 
    let odds = lookupOdds gender  
    randN <- liftIO $ randomPickWithinOdds odds    
    
    if randN == odds then
        pure $ Person gender 
    else
        mzero -- MaybeT $ pure Nothing        


greatGrandparent :: Person -> Gender -> Gender -> Gender -> IO (Maybe Person) 
greatGrandparent person parentGender grandparentGender greatGrandparentGender = do
    lookupParent person parentGender >>= 
        (\ mParent -> 
            case mParent of 
                Just parent -> 
                    lookupParent parent grandparentGender >>= 
                        (\ mGrandparent -> 
                            case mGrandparent of 
                                Just grandparent -> 
                                    lookupParent grandparent greatGrandparentGender
                
                                Nothing -> 
                                    pure Nothing
                        )

                Nothing -> 
                    pure Nothing
        ) 


greatGrandparent1 :: Person -> Gender -> Gender -> Gender -> IO (Maybe Person) 
greatGrandparent1 person parentGender grandparentGender greatGrandparentGender =
    runMaybeT $ do
        parent <- lookupParent1 person parentGender
        grandparent <- lookupParent1 parent grandparentGender
        lookupParent1 grandparent greatGrandparentGender
            

grandparent :: Person -> Gender -> Gender -> IO (Maybe Person) 
grandparent person parentGender grandparentGender =
    lookupParent person parentGender >>= 
        (\ mParent -> mLookupParent mParent grandparentGender)
        -- (\ mParent -> 
        --     case mParent of 
        --         Just parent -> 
        --             lookupParent parent grandparentGender

        --         Nothing -> 
        --             pure Nothing
        -- )

grandparent1 :: Person -> Gender -> Gender -> IO (Maybe Person) 
grandparent1 person parentGender grandparentGender =
    runMaybeT $ do
        parent <- lookupParent1 person parentGender
        lookupParent1 parent grandparentGender


grandfather :: Person -> Gender -> IO (Maybe Person) 
grandfather person parentGender =
    grandparent person parentGender Male


grandfather1 :: Person -> Gender -> IO (Maybe Person) 
grandfather1 person parentGender =
    grandparent1 person parentGender Male 


paternalGrandfather :: Person -> IO (Maybe Person) 
paternalGrandfather person = 
    grandfather person Male


maternalGrandfather :: Person -> IO (Maybe Person) 
maternalGrandfather person =
    grandfather person Female


paternalGrandfather1 :: Person -> IO (Maybe Person) 
paternalGrandfather1 person =
    grandfather1 person Male  


maternalGrandfather1 :: Person -> IO (Maybe Person) 
maternalGrandfather1 person =
    grandfather1 person Female


bothGrandfathers :: Person -> IO (Maybe Person, Maybe Person)
bothGrandfathers person = do
    p <- paternalGrandfather person
    m <- maternalGrandfather person
    pure (p, m)


bothGrandfathers1 :: Person -> IO (Maybe Person, Maybe Person)
bothGrandfathers1 person = do
    p <- paternalGrandfather1 person
    m <- maternalGrandfather1 person
    pure (p, m)

    
-- lookupFather :: Person -> IO (Maybe Person)    
-- lookupFather x =
--     lookupParent x Male


-- lookupFather1 :: Person -> MaybeT IO Person    
-- lookupFather1 x =
--     lookupParent1 x Male


-- lookupMother :: Person -> IO (Maybe Person)    
-- lookupMother x =
--     lookupParent x Female


-- lookupMother1 :: Person -> MaybeT IO Person 
-- lookupMother1 x =
--     lookupParent1 x Female