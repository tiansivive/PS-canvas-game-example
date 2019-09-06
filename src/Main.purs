module Main where

import Prelude

import Board as Board
import Control.Monad.State.Trans (StateT, lift, runStateT)
import Character (Character, Direction(..))
import Character as Character
import Effect (Effect)
import Effect.Console (log)
import Effect.Events (onKeyDown)


handleKeyPress :: StateT Character Effect Unit
handleKeyPress = do
    _ <- lift $ log "key pressed" 
    c <- Character.move Up
    lift $ Character.draw c


main :: Effect Unit
main = do
  log "Starting Up!!"
  b <- Board.createBoard
  c <- Character.create
  Board.draw b
  Character.draw c

  res <- runStateT (Character.set c) c

  onKeyDown "ArrowUp" handleKeyPress
  log "finish setting up"