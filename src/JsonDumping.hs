module JsonDumping where

import Database.HDBC
import Database.HDBC.Sqlite3
import DatabaseActions

-- | creates a JSON representation of Players table
dumpPlayersJSON :: IO ()
dumpPlayersJSON = do
  conn <- connectSqlite3 "footballData.db"
  res <- quickQuery' conn
           "SELECT * from Players" []
  toJSON "players.JSON" (map convertPlayers res)
  disconnect conn

-- | creates a JSON representation of Home Statistics table
dumpHomeStatisticsJSON :: IO ()
dumpHomeStatisticsJSON = do
  conn <- connectSqlite3 "footballData.db"
  res <- quickQuery' conn
           "SELECT * from Home_Statistics" []
  toJSON "home_statistics.JSON" (map convertHome_Statistics res)
  disconnect conn

toJSON :: Show a => FilePath -> a -> IO ()
toJSON file x = writeFile file (show x)
