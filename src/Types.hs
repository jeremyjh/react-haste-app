{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveDataTypeable, Rank2Types #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Haste.App (JSString, Binary(..), Remote, Server)
import Haste.Prim (toJSStr, fromJSStr)

import Control.Applicative ((<$>), pure)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Lens.Family2 (Lens', Traversal')


data Status = Active | Completed
    deriving (Eq, Generic, Typeable)

instance Binary Status

data Todo = Todo
    { _id :: Int
    , _text :: JSString
    , _status :: Status
    } deriving (Eq, Generic, Typeable)

instance Binary JSString where
    get = toJSStr <$> get
    put js = put (fromJSStr js)

instance Binary Todo

data API = API {
    apiFetchTodos  :: Remote (Server [Todo])
  , apiAddTodo  :: Remote (Todo -> Server (Int, [Todo]))
  , apiDeleteTodo  :: Remote (Int -> Server ())
  , apiToggleTodo  :: Remote (Int -> Server ())
  }

-- Commons
-- this traversal is in lens but lens-family has a weird ix which isn't
-- what we want. definition just copied from lens.
ix' :: Int -> Traversal' [a] a
ix' k f xs0 | k < 0     = pure xs0
            | otherwise = go xs0 k where
    go [] _ = pure []
    go (a:as) 0 = (:as) <$> f a
    go (a:as) i = (a:) <$> (go as $! i - 1)

status :: Lens' Todo Status
status f (Todo i t s) = Todo i t <$> f s

toggleStatus :: Status -> Status
toggleStatus Active = Completed
toggleStatus Completed = Active
