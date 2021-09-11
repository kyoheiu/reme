{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Data.List (unwords)
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Dhall
import GHC.Generics
import Network.HTTP.Req
import System.Environment (getArgs)
import Prelude hiding (error)

help =
  "A simple CLI to create slack reminder.\n\
  \To set reminder,\n\
  \`reme \"[text]\" \"[time]\"` will work.\n\
  \`reme \"[text]\"` will ask you the time to send reminder.\n\
  \`reme` shows this help."

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

doWith1Arg :: IO String
doWith1Arg = askTime >> getLine

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
        Success errorRes -> liftIO $ putStrLn $ "Error: " ++ error errorRes
        Error _ -> liftIO $ print "Error. Please try again."

main :: IO ()
main =
  getArgs >>= \args ->
    if null args
      then putStrLn help
      else case length args of
        1 ->
          let task = head args
           in doWith1Arg >>= \t -> registerReq task t
        2 ->
          let [task, t] = args
           in registerReq task t
        _ -> putStrLn "Invalid arguments."
