-- https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe 


greet :: IO ()                        -- type:
greet = do 
    putStr "What is your name? "      -- IO ()
    n <- getLine                      -- IO String
    putStrLn $ "Hello, " ++ n         -- IO ()

           
--mgreet :: MaybeT IO ()
--mgreet = do putStr "What is your name? "    -- IO monad - need MaybeT IO here          


mgreet :: MaybeT IO ()                     -- types:
mgreet = do 
    liftIO $ putStr "What is your name? "  -- MaybeT IO ()
    n <- liftIO getLine                    -- MaybeT IO String
    liftIO $ putStrLn $ "Hello, " ++ n     -- MaybeT IO ()


askfor :: String -> IO String
askfor prompt = do
    putStr $ "What is your " ++ prompt ++ "? "
    getLine

    
survey :: IO (String,String)
survey = do 
    n <- askfor "name"
    c <- askfor "favorite color"
    pure (n,c)    


askfor1 :: String -> IO (Maybe String)
askfor1 prompt = do
    putStr $ "What is your " ++ prompt ++ " (type END to quit)? "
    r <- getLine

    if r == "END"
        then pure Nothing
        else pure (Just r)


survey1 :: IO (Maybe (String, String))
survey1 = do
    ma <- askfor1 "name"
    case ma of
        Nothing -> pure Nothing
        Just n  -> do 
            mc <- askfor1 "favorite color"
            
            case mc of
                Nothing -> pure Nothing
                Just c  -> pure (Just (n,c))            


askfor2 :: String -> MaybeT IO String
askfor2 prompt = do
    liftIO $ putStr $ "What is your " ++ prompt ++ " (type END to quit)? "
    r <- liftIO getLine
    
    if r == "END"
        -- then MaybeT (return Nothing)    -- has type: MaybeT IO String
        -- else MaybeT (return (Just r))   -- has type: MaybeT IO String                
        then mzero
        else pure r


survey2 :: IO (Maybe (String, String))
survey2 = 
    runMaybeT $ do
        name <- askfor2 "name"
        color <- askfor2 "favorite color"
        pure (name, color)
            