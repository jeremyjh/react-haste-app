module Server where

import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C


main :: IO ()
main =
  runApp (mkConfig "ws://localhost:8891" 8891) $ do
    -- Create our state-holding elements
    state <- liftServerIO $ do
      clients <- newIORef []
      messages <- newIORef []
      return (clients, messages)

    -- Create an API object holding all available functions
    api <- API <$> remote (hello state)
               <*> remote (send state)
               <*> remote (await state)

    -- Launch the client
    runClient $ clientMain api
