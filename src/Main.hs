{-# LANGUAGE CPP #-}

module Main where

import Types
import Client (clientMain)

import Haste.App
import Haste.App.Concurrent
import Control.Applicative
import qualified Control.Concurrent as C

type State = String

-- | Tell the server we're here and remove any stale sessions.
send :: Server State -> String -> Server ()
send stateS msg =
 do state <- stateS
    liftIO $ putStrLn (state ++ msg)

main :: IO ()
main = do
  putStrLn "starting ok"
  runApp (mkConfig "ws://localhost:8891" 8891) $ do
    -- Create our state-holding elements
    state <- liftServerIO $
        return "{Begin}"

    -- Create an API object holding all available functions
    api <- API <$> remote (send state)

    -- Launch the client
    runClient $ clientMain api
