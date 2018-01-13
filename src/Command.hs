{-# LANGUAGE OverloadedStrings #-}

module Command where

import ReadPCombinators
import Direction (Direction)
import qualified Direction
import Text.ParserCombinators.ReadP

data Command = Move
             | TurnLeft
             | TurnRight
             | Report
             | Place { x :: Int, y :: Int, facing :: Direction }
  deriving Show

readP :: ReadP Command
readP = choice
  [ readStringAs "move" Move
  , readStringAs "left" TurnLeft
  , readStringAs "right" TurnRight
  , readStringAs "report" Report
  , readPlace
  ]
  where
    readPlace = do
      string "place "
      x <- fmap read (many1 digit)
      string ","
      y <- fmap read (many1 digit)
      string ","
      facing <- Direction.readP
      return Place { x = x, y = y, facing = facing }
