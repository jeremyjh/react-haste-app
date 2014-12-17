{-# LANGUAGE OverloadedStrings, NamedFieldPuns, Rank2Types #-}

module Client (clientMain) where

import Types
import Haste.App

-- TODO:
-- * persistence
-- * routing

import Control.Applicative
import Control.Monad
import Prelude hiding (div)

import Haste.Foreign
import Haste.Prim
import Lens.Family2
import React

import System.IO.Unsafe

import Debug.Trace
-- MODEL

data PageState = PageState
    { _todos :: [Todo]
    , _typingValue :: JSString
    }

-- UTILITY


todos :: Lens' PageState [Todo]
todos f (PageState t v) = (`PageState` v) <$> f t

typingValue :: Lens' PageState JSString
typingValue f (PageState t v) = PageState t <$> f v


trim :: JSString -> JSString
trim = unsafePerformIO . ffi "(function(str) { return str.trim(); })"


-- remove an item from the list by index
iFilter :: Int -> [a] -> [a]
iFilter 0 (_:as) = as
iFilter n (a:as) = a : iFilter (n-1) as
iFilter _ [] = error "can't remove from empty list"

-- CONTROLLER

handleEnter :: PageState -> PageState
handleEnter oldState@PageState{_todos, _typingValue} =
    let trimmed = trim (trace "got here" _typingValue)
    in if trimmed == ""
           then oldState
           else PageState (_todos ++ [Todo 0 trimmed Active]) ""

-- TODO exit editing
-- "If escape is pressed during the edit, the edit state should be left and
-- any changes be discarded."
handleEsc :: PageState -> PageState
handleEsc state = state & typingValue .~ ""

handleHeaderKey :: PageState -> KeyboardEvent -> PageState
handleHeaderKey state KeyboardEvent{key="Enter"} = handleEnter state
handleHeaderKey state KeyboardEvent{key="Escape"} = handleEsc state
handleHeaderKey state _ = state

handleTyping :: PageState -> ChangeEvent -> PageState
handleTyping state (ChangeEvent _typingValue) = state{_typingValue}

statusOfToggle :: [Todo] -> Status
statusOfToggle _todos =
    let allActive = all (\Todo{_status} -> _status == Active) _todos
    in if allActive then Active else Completed

handleToggleAll :: PageState -> MouseEvent -> PageState
handleToggleAll state@PageState{_todos} _ = state{_todos=newTodos} where
    _status = toggleStatus $ statusOfToggle _todos
    newTodos = map (\todo -> todo{_status}) _todos

handleItemCheck :: Int -> PageState -> MouseEvent -> PageState
handleItemCheck todoNum state _ =
    state & todos . ix' todoNum . status %~ toggleStatus

-- TODO
handleLabelDoubleClick :: PageState -> MouseEvent -> PageState
handleLabelDoubleClick = const

handleDestroy :: Int -> PageState -> MouseEvent -> PageState
handleDestroy todoNum state _ = state & todos %~ iFilter todoNum

clearCompleted :: PageState -> MouseEvent -> PageState
clearCompleted state _ = state & todos %~ todosWithStatus Active

-- VIEW

-- "New todos are entered in the input at the top of the app. The input
-- element should be focused when the page is loaded preferably using the
-- autofocus input attribute. Pressing Enter creates the todo, appends it
-- to the todo list and clears the input. Make sure to .trim() the input
-- and then check that it's not empty before creating a new todo."
header :: StatefulReact PageState ()
header = header_ <! id_ "header" $ do
    PageState{_typingValue} <- getState
    h1_ "todos"
    input_ <! id_ "new-todo"
           <! placeholder_ "What needs to be done?"
           <! autofocus_ True
           <! value_ _typingValue
           <! onChange handleTyping
           <! onKeyDown handleHeaderKey

todoView :: Int -> StatefulReact PageState ()
todoView i = do
    PageState{_todos} <- getState
    let Todo{_id, _text, _status} = _todos !! i
    li_ <! class_ (if _status == Completed then "completed" else "") $ do
        div_ <! class_ "view" $ do
            input_ <! class_ "toggle"
                   <! id_ (toJSString $ "toggle-" ++ show _id )
                   <! type_ "checkbox"
                   <! checked_ (_status == Completed)
                   <! onClick (handleItemCheck i)
            label_ <! onDoubleClick handleLabelDoubleClick $ text_ _text
            button_ <! class_ "destroy"
                    <! id_ (toJSString $ "destroy-" ++ show _id)
                    <! onClick (handleDestroy i) $ return ()

        input_ <! class_ "edit"
               <! value_ _text

todosWithStatus :: Status -> [Todo] -> [Todo]
todosWithStatus stat = filter (\Todo{_status} -> _status == stat)

mainBody :: StatefulReact PageState ()
mainBody = do
    PageState{_todos} <- getState
    section_ <! id_ "main" $ do
        input_ <! id_ "toggle-all" <! type_ "checkbox"
        label_ <! for_ "toggle-all"
               <! onClick handleToggleAll $
            "Mark all as complete"

        ul_ <! id_ "todo-list" $ forM_ [0 .. length _todos - 1] todoView

innerFooter :: StatefulReact PageState ()
innerFooter = footer_ <! id_ "footer" $ do
    PageState{_todos} <- getState
    let activeCount = length (todosWithStatus Active _todos)
    let inactiveCount = length (todosWithStatus Completed _todos)

    -- "Displays the number of active todos in a pluralized form. Make sure
    -- the number is wrapped by a <strong> tag. Also make sure to pluralize
    -- the item word correctly: 0 items, 1 item, 2 items. Example: 2 items
    -- left"
    span_ <! id_ "todo-count" $ do
        strong_ (text_ (toJSStr (show activeCount)))

        if activeCount == 1 then " item left" else " items left"

    unless (inactiveCount == 0) $
        button_ <! id_ "clear-completed"
                <! onClick clearCompleted $
            text_ (toJSStr ("Clear completed (" ++ show inactiveCount ++ ")"))

outerFooter :: StatefulReact PageState ()
outerFooter = footer_ <! id_ "info" $ do
    p_ "Double-click to edit a todo"
    p_ $ do
        "Created by "
        a_ <! href_ "http://joelburget.com" $ "Joel Burget"
    p_ $ do
        "Part of "
        a_ <! href_ "http://todomvc.com" $ "TodoMVC"

wholePage :: StatefulReact PageState ()
wholePage = div_ $ do
    PageState{_todos} <- getState
    section_ <! id_ "todoapp" $ do
        header
        -- "When there are no todos, #main and #footer should be hidden."
        unless (null _todos) $ do
            mainBody
            innerFooter
    outerFooter

clientMain :: API -> Client ()
clientMain api = do
    initTodos <- onServer $ apiFetchTodos api
    inject <- liftIO $ do
        Just inject <- elemById "inject"
        render (PageState initTodos "") inject wholePage
        return inject
    let listenMods _id =
             do withElem ("destroy-" ++ show _id ) $ \ todo ->
                    todo `onEvent` OnClick $ \ _ _ ->
                        onServer $ apiDeleteTodo api <.> _id
                withElem ("toggle-" ++ show _id) $ \ todo ->
                    todo `onEvent` OnClick $ \ _ _ ->
                        onServer $ apiToggleTodo api <.> _id
    forM_ initTodos $ \Todo{_id} -> listenMods _id
    withElem "new-todo" $ \ todo ->
        todo `onEvent` OnKeyDown $ \k ->
            case k of
                13 -> do
                    m <- getProp todo "value"
                    (_id, todos') <- onServer $ apiAddTodo api <.> Todo 0 (toJSString m) Active
                    liftIO $
                        render (PageState todos' "") inject wholePage
                    listenMods _id
                _ -> return ()
