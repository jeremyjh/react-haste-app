{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server (initAPI) where

import Haste.App
       (App, Server, remote, liftServerIO, JSString)
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

deleteTodo :: Int -> Update State State
deleteTodo 0 =
 do todos <- State.get
    let todos' = drop 1 todos
    State.put todos'
    return todos'
deleteTodo i =
 do todos <- State.get
    let todos' = take 1 todos ++ drop (i + 1) todos
    State.put todos'
    return todos'

$(makeAcidic ''State ['allTodos, 'addTodo, 'deleteTodo])

fetchTodos :: Server (AcidState State) -> Server State
fetchTodos state' =
 do state <- state'
    query' state AllTodos

insertTodo :: Server (AcidState State) -> Todo -> Server State
insertTodo state' todo =
 do state <- state'
    update' state (AddTodo todo)

removeTodo :: Server (AcidState State) -> Int -> Server State
removeTodo state' i =
 do state <- state'
    update' state (DeleteTodo i)

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
        <*> remote (removeTodo state)
  where initialTodos =
            [Todo "derp" Active, Todo "xyz" Completed
            ,Todo "sjdfk" Active, Todo "ksljl" Completed]

deriveSafeCopy 1 'base ''JSString
deriveSafeCopy 1 'base ''Status
deriveSafeCopy 1 'base ''Todo
