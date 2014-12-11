{-# LANGUAGE CPP #-}

module Main where

import Types
import Client (clientMain)

import Haste.App
import Haste.App.Concurrent
import Control.Applicative
import qualified Control.Concurrent as C

#ifdef __HASTE__
initAPI :: App API
initAPI =
    API <$> remote (error "cannot evaluate initAPI in Client!")
        <*> remote (error "cannot evaluate initAPI in Client!")
#else
import Server(initAPI)
#endif

main :: IO ()
main =
 do let port = 8891
    let state = "{Begin}"
    putStrLn ("Starting Haste.App on port " ++ show port)
    runApp (mkConfig ("ws://localhost:" ++ show port) port) $
     do api <- initAPI
        runClient $ clientMain api
