{-# LANGUAGE CPP #-}

module Main where

import Client (clientMain)

import Haste.App

#ifdef __HASTE__
import Types(API(..))
import Control.Applicative ((<*>), (<$>))

initAPI :: App API
initAPI =
    API <$> remote nohaste
        <*> remote nohaste
        <*> remote nohaste
  where nohaste = (error "cannot evaluate initAPI in Client!")
#else
import Server(initAPI)
#endif

main :: IO ()
main =
    let port = 8891 in
 do putStrLn ("Starting Haste.App on port " ++ show port)
    runApp (mkConfig ("ws://localhost:" ++ show port) port) $
     do api <- initAPI
        runClient $ clientMain api
