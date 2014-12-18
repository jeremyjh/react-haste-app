{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server (initAPI) where

import Haste.App
       (App, Server, remote, liftServerIO, JSString)
import Types

import Control.Applicative ((<*>), (<$>))

import Control.Monad.Reader(ask)

import Data.Acid (Query, Update, makeAcidic, AcidState, openLocalStateFrom)
import Data.Acid.Advanced (update', query')
import Data.SafeCopy (deriveSafeCopy, base)

import qualified Data.IntMap.Strict as Map

import Lens.Family2 (Lens', views, over, (^.))
import Lens.Family2.State ((%=), (+=), use, uses)

data AppState = AppState {_counter :: Int, _todos :: Map.IntMap Todo}

counter :: Lens' AppState Int
counter f (AppState c t) = (`AppState` t) <$> f c

todos :: Lens' AppState (Map.IntMap Todo)
todos f (AppState c t) = AppState c <$> f t

allTodos :: Query AppState [Todo]
allTodos = views todos (map snd . Map.toList) <$> ask

addTodo :: Todo -> Update AppState (Int, [Todo])
addTodo todo =
 do counter += 1
    counter' <- use counter
    todos %= Map.insert counter' todo {_id = counter'}
    (,) counter' <$> uses todos (map snd . Map.toList)

deleteTodo :: Int -> Update AppState ()
deleteTodo i = todos %= Map.delete i

toggleTodo :: Int -> Update AppState ()
toggleTodo i =
    todos %= Map.update toggleM i
  where toggleM = Just . over status toggleStatus

clearComplete :: Update AppState [Todo]
clearComplete =
 do todos %= Map.filter (\t -> t ^. status /= Completed )
    map snd . Map.toList <$> use todos

$(makeAcidic ''AppState ['allTodos, 'addTodo, 'deleteTodo, 'toggleTodo, 'clearComplete])

fetchTodos :: Server (AcidState AppState) -> Server [Todo]
fetchTodos state' =
 do state <- state'
    query' state AllTodos

insertTodo :: Server (AcidState AppState) -> Todo -> Server (Int, [Todo])
insertTodo state' todo =
 do state <- state'
    update' state (AddTodo todo)

removeTodo :: Server (AcidState AppState) -> Int -> Server ()
removeTodo state' i =
 do state <- state'
    update' state (DeleteTodo i)

doToggleTodo :: Server (AcidState AppState) -> Int -> Server ()
doToggleTodo state' i =
 do state <- state'
    update' state (ToggleTodo i)

doClearComplete :: Server (AcidState AppState) -> Server [Todo]
doClearComplete state' =
 do state <- state'
    update' state ClearComplete

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
        <*> remote (doToggleTodo state)
        <*> remote (doClearComplete state)
  where initialTodos = AppState 4 $ Map.fromList
            [(1, Todo 1 "one" Active), (2, Todo 2 "two" Completed)
            ,(3, Todo 3 "three" Active), (4, Todo 4 "four" Completed)]

deriveSafeCopy 1 'base ''JSString
deriveSafeCopy 1 'base ''Status
deriveSafeCopy 1 'base ''AppState
deriveSafeCopy 1 'base ''Todo
