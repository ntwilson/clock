module Lib (someFunc) where

import Paths_Clock (getDataFileName)

import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)
import System.IO (hFlush, stdout)
import GHC.IO.Exception (ExitCode(..))
import Data.List.Split (splitOn)
import qualified Data.List as List
import qualified Safe as List
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)

timesheetRelativePath :: String
timesheetRelativePath = "data\\timesheet.csv"

timesheetPath :: IO FilePath
timesheetPath = getDataFileName timesheetRelativePath

timesheetContents :: IO [[String]]
timesheetContents = do
  filePath <- timesheetPath
  fileLines <- lines <$> readFile filePath 
  pure $ splitOn "," <$> fileLines

appendRow :: [String] -> IO ()
appendRow row = do
  let line = List.concat row
  filePath <- timesheetPath
  appendFile filePath $ line

data ActionType = ClockIn | ClockOut | Launch | Clear | Unrecognized

action :: [String] -> ActionType
action ("in":_) = ClockIn
action ("out":_) = ClockOut
action ("open":_) = Launch
action ("clear":_) = Clear
action _ = Unrecognized

timeOfClockIn :: IO (Maybe Time.ZonedTime)
timeOfClockIn = do
  contents <- timesheetContents
  let date = strToDate =<< List.headMay =<< List.lastMay contents
  pure date

  where
    strToDate dateStr =
      iso8601ParseM dateStr

  
clockedOut :: [[String]] -> Bool
clockedOut fileLines = 
  case List.lastMay fileLines of
    Just last -> List.length last > 1
    Nothing -> True
    
clockedIn :: [[String]] -> Bool
clockedIn = not . clockedOut

clockIn :: IO (Either String String)
clockIn = do
  contents <- timesheetContents
  if clockedIn contents 
  then pure $ Left "didn't do anything: you're already clocked in!"
  else do
    now <- Time.getZonedTime
    appendRow ["\n" ++ iso8601Show now]
    pure $ Right $ "Clocked in at: " ++ iso8601Show now

clockOut :: IO (Either String String)
clockOut = do
  contents <- timesheetContents
  if clockedOut contents
  then pure $ Left "didn't do anything: you aren't clocked in!"
  else do
    now <- Time.getZonedTime
    maybeInTime <- timeOfClockIn
    case maybeInTime of 
      Just inTime -> do
        let 
          diff = Time.diffUTCTime (Time.zonedTimeToUTC now) (Time.zonedTimeToUTC inTime)
          diffStr = Time.formatTime Time.defaultTimeLocale "%h:%M%ES" diff

        appendRow [",", iso8601Show now, ",", diffStr]
        pure $ Right $ "Clocked out; elapsed time: " ++ diffStr

openTimesheet :: IO (Either String String)
openTimesheet = do
  exitCode <- waitForProcess =<< runCommand =<< timesheetPath
  case exitCode of
    ExitSuccess -> pure $ Right "done"
    ExitFailure code -> pure $ Left $ "Exit code: " ++ show code


clearTimesheet :: IO ()
clearTimesheet = do
  putStrLn "Are you sure you want to clear the timesheet?"
  putStr "(y/n):"
  hFlush stdout
  ans <- getLine
  case ans of 
    "y" -> do
      filePath <- timesheetPath
      writeFile filePath "Clocked In:,Clocked Out:,Duration:"
    _ -> pure ()


someFunc :: IO ()
someFunc = do
  args <- getArgs
  result <- 
    case action args of
      ClockIn -> clockIn
      ClockOut -> clockOut
      Launch -> openTimesheet
      Clear -> do
        clearTimesheet
        pure $ Right "done"
      Unrecognized -> pure $ Left "huh?"

  case result of 
    Right str -> putStrLn str
    Left str -> putStrLn $ "ERR: " ++ str

  