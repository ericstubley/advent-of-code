{-# LANGUAGE OverloadedStrings #-}

module Automation where

import Control.Lens
import Network.Wreq
import Network.Wreq.Types (Postable)
import System.IO
import Formatting
import Cookie
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Internal



baseURLFormatter    = formatToString ("https://adventofcode.com/" % int % "/day/" % int)
inputURLFormatter   = formatToString ("https://adventofcode.com/" % int % "/day/" % int % "/input")
submitURLFormatter  = formatToString ("https://adventofcode.com/" % int % "/day/" % int % "/answer")


runGetRequest :: String -> IO (Response BS.ByteString)
runGetRequest url = do
    let opts = defaults & header "Cookie" .~ [Cookie.cookie]
    response <- getWith opts url
    return response


runPostRequest :: (Postable a) => String -> a -> IO (Response BS.ByteString)
runPostRequest url postArgs = do
    let opts = defaults & header "Cookie" .~ [Cookie.cookie]
    response <- postWith opts url postArgs
    return response


getPuzzle :: Int -> Int -> IO String
getPuzzle year day = do
    let puzzleURL = baseURLFormatter year day
    response <- runGetRequest puzzleURL
    return $ show (response ^. responseBody)
    

getInput :: Int -> Int -> IO String
getInput year day = do
    let inputURL = inputURLFormatter year day
    response <- runGetRequest inputURL
    return $ show (response ^. responseBody)


inputToFile :: String -> Int -> Int -> IO ()
inputToFile filepath year day = do
    let inputURL = inputURLFormatter year day
    response <- runGetRequest inputURL
    writeFile filepath (show (response ^. responseBody))
    return ()


submitAnswer :: (Show a) => Int -> Int -> Int -> a -> IO String
submitAnswer year day level answer = do
    let fAnswer = formattedAnswer answer
    putStrLn ("Are you sure you want to submit " ++ fAnswer ++ " for " ++ 
            show year ++ " day " ++ show day ++ " part "  ++ show level ++ "?")
    confirm <- getLine
    if confirm == "y"
        then do
            let submitURL = submitURLFormatter year day
            let value = "Value" :: String
            let postArgs = ["Name" := value, "level" := show level, "answer" := fAnswer]
            response <- runPostRequest submitURL postArgs
            return $ show (response ^. responseBody)
        else do
            return "Not submitted"

-- this is super janky and there's better Haskell-y typeclass ways to do it
formattedAnswer :: (Show a) => a -> String
formattedAnswer x = lStrip . rStrip . show $ x where
    lStrip s
        | head s == '"' = tail s
        | otherwise     = s
    rStrip s 
        | last s == '"' = init s
        | otherwise     = s


