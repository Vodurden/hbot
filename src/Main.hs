{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import ReadPCombinators
import Command
import Robot
import World
import Direction
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

doCommand :: Command -> World -> World
doCommand Place { x, y, facing } world =
  let robot = Robot { x = x, y = y, facing = facing }
  in world { maybeRobot = Just robot }
doCommand Move world @ World { maybeRobot = Just (robot @ Robot { y = oldY, facing = North }) } =
  world { maybeRobot = Just (robot { Robot.y = oldY + 1 }) }
doCommand Move world @ World { maybeRobot = Just (robot @ Robot { x, y, facing = East }) } =
  world { maybeRobot = Just (robot { Robot.x = x + 1 }) }
doCommand Move world @ World { maybeRobot = Just (robot @ Robot { x, y, facing = South }) } =
  world { maybeRobot = Just (robot { Robot.y = y - 1 }) }
doCommand Move world @ World { maybeRobot = Just (robot @ Robot { x, y, facing = West }) } =
  world { maybeRobot = Just (robot { Robot.x = x - 1 }) }
doCommand TurnLeft world @ World { maybeRobot = Just (robot @ Robot { facing }) } =
  world { maybeRobot = Just (robot { Robot.facing = left facing }) }
doCommand TurnRight world @ World { maybeRobot = Just (robot @ Robot { facing }) } =
  world { maybeRobot = Just (robot { Robot.facing = right facing }) }
doCommand _ world = world

doMessage :: Command -> World -> Maybe T.Text
doMessage Report World { maybeRobot = Just robot } = Just (T.pack $ show robot)
doMessage _ _ = Nothing

isValid :: World -> Bool
isValid World { maybeRobot = Just robot } = let validX = (Robot.x robot) >= 0 && (Robot.x robot) < 5
                                                validY = (Robot.y robot) >= 0 && (Robot.y robot) < 5
                                                validRobot = validX && validY
                                            in validRobot
isValid World { maybeRobot = Nothing } = True

runGame :: StateT World IO ()
runGame = do
  currentWorld <- get
  input <- io T.getLine

  let maybeCommand = parseCommand (T.toLower input)
  io $ print maybeCommand

  case maybeCommand of
    Just command -> do
      let newWorld = doCommand command currentWorld
      if isValid newWorld then do
        let maybeMessage = doMessage command newWorld
        put newWorld

        maybe (io $ return ()) (io . print) maybeMessage
      else
        io $ print "Invalid state"
    Nothing -> io $ print "Invalid command"
  where
    io = liftIO
    parseCommand = parseMaybe Command.readP

main :: IO ()
main =
  do
    runGameLoop initialWorld
    return ()
  where
    runGameLoop = iterateM_ (\robot -> execStateT runGame robot)
    initialWorld = World { maybeRobot = Nothing }
