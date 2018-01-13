{-# LANGUAGE OverloadedStrings #-}

module Direction where

import ReadPCombinators
import Text.ParserCombinators.ReadP

data Direction = North | East | South | West
  deriving Show

left :: Direction -> Direction
left North = West
left West = South
left South = East
left East = North

right :: Direction -> Direction
right North = East
right East = South
right South = West
right West = North

readP :: ReadP Direction
readP = choice
  [ readStringAs "north" North
  , readStringAs "east" East
  , readStringAs "south" South
  , readStringAs "west" West
  ]
