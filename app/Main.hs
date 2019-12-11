{-# LANGUAGE OverloadedStrings #-}
module Main where

import ApiRequest
import DatabaseActions
import Data.ByteString.Char8 as D
import JsonDumping
import Data.String

main :: IO ()
main = do
  response <- api_call
  let football_data = unpack response
  batchInsert football_data
  dumpPlayersJSON
  dumpHomeStatisticsJSON
  D.putStrLn "\nWELCOME TO OUR PROGRAM!"
  D.putStrLn "Please check project folder to find JSON representation of Haskell data. \n"
  userQueries

-- | awaits user response before executing queries
userQueries :: IO ()
userQueries = do
   D.putStrLn "\nPRESS 'q' TO QUERY THE DATABASE, 'd' TO DELETE DATA FROM THE DATABASE OR ANY OTHER LETTER TO EXIT"
   task <- D.getLine
   case task of
      "q" -> query
      "d" -> delete
      _ -> return ()

-- | deletes data from Players table
delete :: IO ()
delete = do
   deleteIdlePlayers
   D.putStrLn "Delete EXECUTED"
   userQueries

-- | offers users flexibility to select query of choice
query :: IO ()
query = do
   D.putStrLn "BELOW ARE THE AVAILABLE QUERIES, PLEASE SELECT THE CORRESPONDING LETTER:"
   D.putStrLn "a. Defenders Involvement\nb. Goalkeeper with most clean sheets\nc. English Players\nd. Most Involved\ne. French Players\nf. Players with less than 600 minutes played"
   D.putStrLn "OR IF YOU WOULD LIKE TO RETURN TO PREVIOUS SELECTION PRESS ANY OTHER LETTER"
   chosenQuery <- D.getLine
   case chosenQuery of
      "a" -> defendersInvolvement
      "b" -> mostCleanSheets
      "c" -> englishPlayersHighStats
      "d" -> mostInvolved
      "e" -> frenchPlayers
      "f" -> minutesPlayed
      _ -> D.putStrLn "Invalid"
   userQueries
