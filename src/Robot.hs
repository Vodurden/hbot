module Robot where

import Direction

data Robot = Robot
  { x :: Int
  , y :: Int
  , facing :: Direction
  }
  deriving Show
