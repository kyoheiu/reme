{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.IO.Class
import Network.HTTP.Req
import Data.Aeson
import Dhall
import GHC.Generics
import qualified Data.ByteString as BS
import Data.ByteString.UTF8

newtype Config = Config
    { slackToken:: String
    } deriving (Show, Generic)

data MyData = MyData
    { text:: String
    , time:: String
    } deriving (Show, Generic)

instance FromDhall Config
instance ToJSON MyData

path = "~/.config/reme.dhall"

addPerson p as
    | elem p as || p == "" = p
    | otherwise = ""

main :: IO ()
main = do
    config <- input auto path
    putStrLn "What's your task?"
    task <- getLine
    putStrLn "And when to send reminder?"
    putStrLn "e.g. \"in 5 minutes\" or \"Every Thursday\""
    t <- getLine
    let myData = MyData { text = task, time = t }
    runReq defaultHttpConfig $ do
        v <- req POST (https "slack.com" /: "api" /: "reminders.add") (ReqBodyJson myData) jsonResponse $
            oAuth2Bearer $ fromString $ slackToken config
        liftIO $ print (responseBody v:: Value)
