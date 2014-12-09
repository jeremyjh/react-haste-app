module Types (API(..)) where

import Haste.App

data API = API {
    apiSend  :: Remote (String -> Server ())
  }
