{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Dhall
import GHC.Generics
import Network.HTTP.Req
import System.Environment (getArgs)
import Prelude hiding (error)

path = "~/.config/reme.dhall"

newtype Config = Config
  { slackToken :: String
  }
  deriving (Show, Generic)

data MyData = MyData
  { text :: String,
    time :: String
  }
  deriving (Show, Generic)

data SlackResponse = SlackResponse
  { ok :: Bool,
    reminder :: Reminder
  }
  deriving (Show, Generic)

data Reminder = Reminder
  { id :: String,
    creator :: String,
    user :: String,
    text :: String,
    recurring :: Bool,
    time :: Int,
    complete_ts :: Double
  }
  deriving (Show, Generic)

data SlackErrorResponse = SlackErrorResponse
  { ok :: Bool,
    error :: String
  }
  deriving (Show, Generic)

instance FromDhall Config

instance ToJSON MyData

instance FromJSON SlackResponse

instance FromJSON Reminder

instance FromJSON SlackErrorResponse

toLocalTime tz =
  show . utcToLocalTime tz . posixSecondsToUTCTime . realToFrac

extractInfo r =
  getCurrentTimeZone >>= \tz ->
    putStrLn $
      "\nOK! task: "
        ++ reText rem
        ++ "\n    time: "
        ++ toLocalTime tz (reTime rem)
  where
    reText :: Reminder -> String
    reText = text
    reTime :: Reminder -> Int
    reTime = time
    rem = reminder r

askTime = do
  putStrLn "When to send reminder?"
  putStrLn "e.g. \"in 5 minutes\" or \"Every Thursday\""

doWith0Arg :: IO (String, String)
doWith0Arg = do
  putStrLn "What's your task?"
  task <- getLine
  askTime
  t <- getLine
  return (task, t)

doWith1Args :: IO String
doWith1Args = askTime >> getLine

registerReq :: String -> String -> IO ()
registerReq task t = do
  config <- input auto path
  let myData = MyData {text = task, time = t}
  runReq defaultHttpConfig $ do
    v <-
      req
        POST
        (https "slack.com" /: "api" /: "reminders.add")
        (ReqBodyJson myData)
        jsonResponse
        $ oAuth2Bearer $ BS8.fromString $ slackToken config
    let resBody = responseBody v
    case fromJSON resBody of
      Success res -> liftIO $ extractInfo res
      Error _ -> case fromJSON resBody of
        Success errorRes -> liftIO $ print $ error errorRes
        Error _ -> liftIO $ print "Error. Please try again."

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> do
      (task, t) <- doWith0Arg
      registerReq task t
    1 -> do
      let task = head args
      t <- doWith1Args
      registerReq task t
    2 -> do
      let [task, t] = args
      registerReq task t
    _ -> putStrLn "Invalid arguments. See `reme --help`"
