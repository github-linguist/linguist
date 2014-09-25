module ReactiveJQueryTest where

import Prelude ((+), (++), (<$>), (<*>), ($), (<<<), flip, return, show)
import Control.Monad
import Control.Monad.Eff
import Control.Monad.JQuery
import Control.Reactive
import Control.Reactive.JQuery
import Data.Array (map, head, length)
import Data.Foldable
import Data.Foreign
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Debug.Trace
import Global (parseInt)

main = do
  personDemo
  todoListDemo

greet firstName lastName = "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

personDemo = do
  -- Create new reactive variables to hold the user's names
  firstName <- newRVar "John"
  lastName <- newRVar "Smith"

  -- Get the document body
  b <- body

  -- Create a text box for the first name
  firstNameDiv <- create "<div>"
  firstNameInput <- create "<input>"
  "First Name: " `appendText` firstNameDiv
  firstNameInput `append` firstNameDiv
  firstNameDiv `append` b

  -- Create a text box for the last name
  lastNameDiv <- create "<div>"
  lastNameInput <- create "<input>"
  "Last Name: " `appendText` lastNameDiv
  lastNameInput `append` lastNameDiv
  lastNameDiv `append` b

  -- Bind the text box values to the name variables
  bindValueTwoWay firstName firstNameInput
  bindValueTwoWay lastName lastNameInput

  -- Create a paragraph to display a greeting
  greeting <- create "<p>"
  { color: "red" } `css` greeting
  greeting `append` b

  -- Bind the text property of the greeting paragraph to a computed property
  let greetingC = greet <$> toComputed firstName <*> toComputed lastName
  bindTextOneWay greetingC greeting

todoListDemo = do
  -- Get the document body
  b <- body

  -- Create an array
  arr <- newRArray

  text1 <- newRVar "Learn PureScript"
  comp1 <- newRVar false
  insertRArray arr { text: text1, completed: comp1 } 0
  
  ul <- create "<ul>"

  -- Bind the ul to the array
  bindArray arr ul $ \entry indexR -> do
    li <- create "<li>"

    completedInput <- create "<input>"
    setAttr "type" "checkbox" completedInput
    completedInput `append` li
    sub1 <- bindCheckedTwoWay entry.completed completedInput
    
    textInput <- create "<input>"
    textInput `append` li
    sub2 <- bindValueTwoWay entry.text textInput

    btn <- create "<button>"
    "Remove" `appendText` btn
    flip (on "click") btn $ do
      index <- readRVar indexR
      removeRArray arr index
    btn `append` li

    return { el: li, subscription: sub1 <> sub2 }

  ul `append` b

  -- Add button
  newEntryDiv <- create "<div>"
  btn <- create "<button>"
  "Add" `appendText` btn
  btn `append` newEntryDiv
  newEntryDiv `append` b

  flip (on "click") btn $ do
    text <- newRVar ""
    completed <- newRVar false
    arr' <- readRArray arr
    insertRArray arr { text: text, completed: completed } (length arr')

  -- Create a paragraph to display the next task
  nextTaskLabel <- create "<p>"
  nextTaskLabel `append` b

  let nextTask = do
    task <- head <$> toComputedArray arr
    case task of
      Nothing -> return "Done!"
      Just { text = text } -> (++) "Next task: " <$> toComputed text
  bindTextOneWay nextTask nextTaskLabel

  -- Create a paragraph to display the task counter
  counterLabel <- create "<p>"
  counterLabel `append` b

  let counter = (flip (++) " tasks remaining") <<< show <$> do
    rs <- toComputedArray arr
    cs <- map (\c -> if c then 0 else 1) <$> traverse (\entry -> toComputed entry.completed) rs
    return $ foldl (+) 0 cs
  bindTextOneWay counter counterLabel
