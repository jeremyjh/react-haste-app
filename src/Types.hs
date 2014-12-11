{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveDataTypeable #-}

module Types where

import Haste.App
import Haste.Prim

import Control.Applicative

import GHC.Generics
import Data.Typeable


data Status = Active | Completed
    deriving (Eq, Generic, Typeable)

instance Binary Status

data Todo = Todo
    { _text :: JSString
    , _status :: Status
    } deriving (Eq, Generic, Typeable)

instance Binary JSString where
    get = toJSStr <$> get
    put js = put (fromJSStr js)

instance Binary Todo

data API = API {
    apiFetchTodos  :: Remote (Server [Todo]),
    apiAddTodo  :: Remote (Todo -> Server [Todo])
  }
