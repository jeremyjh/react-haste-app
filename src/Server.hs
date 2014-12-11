{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server (initAPI) where

import Haste.App (App, Server, remote, liftServerIO, JSString)
import Types

import Control.Applicative ((<*>), (<$>))

import Control.Monad.Reader(ask)
import qualified Control.Monad.State as State

import Data.Acid (Query, Update, makeAcidic, AcidState, openLocalStateFrom)
import Data.Acid.Advanced (update', query')
import Data.SafeCopy (deriveSafeCopy, base)

type State = [Todo]

allTodos :: Query State State
allTodos = ask

addTodo :: Todo -> Update State State
addTodo todo =
 do todos <- State.get
    State.put (todo : todos)
    return (todo : todos)

$(makeAcidic ''State ['allTodos, 'addTodo])

fetchTodos :: Server (AcidState State) -> Server State
fetchTodos state' =
 do state <- state'
    query' state AllTodos

insertTodo :: Server (AcidState State) -> Todo -> Server State
insertTodo state' todo =
 do state <- state'
    update' state (AddTodo todo)

-- | Initialize the Server API, capturing all the remote operations.
--
-- Note: This is the only function we export, making it possible to
-- shield hastec from this module (and its dependencies)
-- with a single mock in Main.hs.
initAPI :: App API
initAPI =
 do state <- liftServerIO $ openLocalStateFrom "db" initialTodos
    API <$> remote (fetchTodos state)
        <*> remote (insertTodo state)
  where initialTodos =
            [Todo "derp" Active, Todo "xyz" Completed
            ,Todo "sjdfk" Active, Todo "ksljl" Completed]

deriveSafeCopy 1 'base ''JSString
deriveSafeCopy 1 'base ''Status
deriveSafeCopy 1 'base ''Todo
