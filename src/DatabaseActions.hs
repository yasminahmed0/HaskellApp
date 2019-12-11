module DatabaseActions
    ( batchInsert,
      minutesPlayed,
      mostCleanSheets,
      frenchPlayers,
      deleteIdlePlayers,
      mostInvolved,
      englishPlayersHighStats,
      defendersInvolvement,
      convertPlayers,
      convertHome_Statistics
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad(when)
import Types
import Data.List

-- | create Player table
createPlayers :: Connection -> IO Integer
createPlayers conn =
   do
       run conn
                "CREATE TABLE IF NOT EXISTS Players (\
                \p_id VARCHAR(500) PRIMARY KEY, \
                \full_name VARCHAR(200) DEFAULT NULL, \
                \age INTEGER DEFAULT NULL, \
                \position VARCHAR(40), \
                \team VARCHAR(40) DEFAULT NULL, \
                \nationality VARCHAR(40) DEFAULT NULL \
                \)"
                []

-- | create Home Statistics table
createStatistics :: Connection -> IO Integer
createStatistics conn =
   do
       run conn
                "CREATE TABLE IF NOT EXISTS Home_Statistics (\
                \player_id VARCHAR(500), \
                \appearances INTEGER DEFAULT NULL, \
                \goals INTEGER DEFAULT NULL, \
                \goals_involved_per_90 DOUBLE DEFAULT NULL, \
                \assists INTEGER DEFAULT NULL, \
                \minutes_played INTEGER DEFAULT NULL, \
                \clean_sheets INTEGER DEFAULT NULL, \
                \goals_conceded INTEGER DEFAULT NULL, \
                \FOREIGN KEY (player_id) REFERENCES Players(p_id) \
                \)"
                []

-- | open database connection and create tables
batchInsert :: String -> IO ()
batchInsert football_data = do
  conn <- connectSqlite3 "footballData.db"
  createPlayers conn
  createStatistics conn
  let format_data = tail $ lines football_data
  let players = map csvToPlayers format_data
  stmt <- insertPlayersIOStmt conn
  executeMany stmt $ map playersToSqlValues players
  let homeStatistics = map csvToStatistics format_data
  stmt2 <- insertStatisticsIOStmt conn
  executeMany stmt2 $ map homeStatisticsToSqlValues homeStatistics
  commit conn

-- | function to insert Player into database
insertPlayersIOStmt :: Connection -> IO Statement
insertPlayersIOStmt conn = prepare conn "INSERT INTO Players VALUES (?,?,?,?,?,?)"

-- | function to insert Home Statistics into database
insertStatisticsIOStmt :: Connection -> IO Statement
insertStatisticsIOStmt conn = prepare conn "INSERT INTO Home_Statistics VALUES (?,?,?,?,?,?,?,?)"

-------------------------------------- QUERIES -----------------------------------------------

-- | Shows all the defenders and their level of involvement with goals conceded
defendersInvolvement :: IO ()
defendersInvolvement = do
    conn <- connectSqlite3 "footballData.db"
    res <- quickQuery' conn
            "SELECT Players.full_name, Players.position, Home_Statistics.goals_conceded \
            \ FROM Players JOIN Home_Statistics ON Players.p_id=Home_Statistics.player_id \
            \ WHERE Players.position = 'Defender' " []
    print (map convertDefendersInvolvement res)
    disconnect conn

-- | Outputs the most involved players in the league, these players have the highest goals involvement per 90 minute average and highest assists.
mostInvolved :: IO ()
mostInvolved = do
    conn <- connectSqlite3 "footballData.db"
    res <- quickQuery' conn
            "SELECT Players.full_name, Home_Statistics.appearances, Home_Statistics.goals_involved_per_90, Home_Statistics.goals, Home_Statistics.assists \
            \ FROM Players JOIN Home_Statistics ON Players.p_id=Home_Statistics.player_id \
            \ WHERE Home_Statistics.goals_involved_per_90 > 0.5 AND Home_Statistics.assists > 3 \
            \ ORDER BY Home_Statistics.appearances DESC" []
    print (map convertMostInvolved res)
    disconnect conn

-- | This function shows all the english players with high apppearances for their team. They are sorted by both appearances and how many clean sheets they have held.
englishPlayersHighStats :: IO ()
englishPlayersHighStats = do
    conn <- connectSqlite3 "footballData.db"
    res <- quickQuery' conn
            "SELECT Players.full_name, Players.nationality, Home_Statistics.appearances, Home_Statistics.clean_sheets \
            \ FROM Players JOIN Home_Statistics ON Players.p_id=Home_Statistics.player_id \
            \ WHERE Home_Statistics.appearances > 12 AND Players.nationality = 'England' \
            \ ORDER BY Home_Statistics.appearances DESC, Home_Statistics.clean_sheets DESC" []
    print (map convertEnglishPlayersHighStats res)
    disconnect conn

-- | Lists all French players
frenchPlayers :: IO ()
frenchPlayers = do
    conn <- connectSqlite3 "footballData.db"
    res <- quickQuery' conn
            "SELECT Players.full_name FROM Players WHERE nationality = 'France'" []
    print (map convertPlayersNames res)
    disconnect conn

-- | This function shows the least active players
minutesPlayed :: IO ()
minutesPlayed = do
   conn <- connectSqlite3 "footballData.db"
   res <- quickQuery' conn
            "SELECT full_name from Players \
             \ INNER JOIN Home_Statistics ON Players.p_id = Home_Statistics.player_id \
             \ WHERE minutes_played <= 600" []
   print (map convertPlayersNames res)
   disconnect conn

-- | This function shows the keeper that has held the most clean sheets in the league
mostCleanSheets :: IO ()
mostCleanSheets = do
   conn <- connectSqlite3 "footballData.db"
   res <- quickQuery' conn
            "SELECT Players.full_name, Players.position, Players.team, Home_Statistics.clean_sheets \
            \ FROM Players INNER JOIN Home_Statistics ON Players.p_id=Home_Statistics.player_id \
            \ WHERE Home_Statistics.clean_sheets >= 12 AND Players.position = 'Goalkeeper' " []
   print (map convertMostCleanSheets res)
   disconnect conn

-- | This function will delete all the players that have played less than 600 minutes in the league
deleteIdlePlayers :: IO ()
deleteIdlePlayers = do
    conn <- connectSqlite3 "footballData.db"
    res <- quickQuery' conn
             "DELETE FROM Home_Statistics WHERE minutes_played <= 600" []
    disconnect conn

----------------------FUNCTIONS TO CONVERT QUERY DATA TO DESIRED FORMAT --------------------------
convertDefendersInvolvement :: [SqlValue] -> String
convertDefendersInvolvement [p_full_name, p_position, p_goals_conceded] =
   "{ Name: "++ ((fromSql p_full_name) :: String) ++
   ", Position: " ++ ((fromSql p_position) :: String) ++
   ", Goals_Conceded: " ++((fromSql p_goals_conceded) :: String) ++ "}"
convertDefendersInvolvement x = error $ "Can't convert query row " ++ show x

convertMostInvolved :: [SqlValue] -> String
convertMostInvolved [s_full_name, s_appearances, s_goals_involved, s_goals, s_assists] =
   "{Name: "++ ((fromSql s_full_name) :: String) ++
   ", Appearances: " ++ ((fromSql s_appearances) :: String) ++
   ", Goals_Involved: " ++((fromSql s_goals_involved) :: String) ++
   ", Goals: " ++((fromSql s_goals) :: String) ++
   ", Assists: " ++((fromSql s_assists) :: String) ++ "}"
convertMostInvolved x = error $ "Can't convert query row " ++ show x

convertEnglishPlayersHighStats :: [SqlValue] -> String
convertEnglishPlayersHighStats [s_full_name, s_nationality, s_appearances, s_clean_sheets] =
   "{Name: "++ ((fromSql s_full_name) :: String) ++
   ", Nationality: " ++ ((fromSql s_nationality) :: String) ++
   ", Appearances: " ++((fromSql s_appearances) :: String) ++
   ", Clean_Sheets: " ++((fromSql s_clean_sheets) :: String) ++ "}"
convertEnglishPlayersHighStats x = error $ "Can't convert query row " ++ show x

convertPlayersNames :: [SqlValue] -> String
convertPlayersNames [s_full_name] =
  "{Name: "++((fromSql s_full_name):: String) ++ "}"
convertPlayersNames x = error $ "Can't convert query row" ++ show x

convertMostCleanSheets :: [SqlValue] -> String
convertMostCleanSheets [s_full_name, s_position, s_team, h_clean_sheets] =
   "{Name: "++ ((fromSql s_full_name) :: String) ++
   ", Position: " ++ ((fromSql s_position) :: String) ++
   ", Team: " ++((fromSql s_team) :: String) ++
   ", Clean_Sheets: " ++((fromSql h_clean_sheets) :: String) ++ "}"
convertMostCleanSheets x = error $ "Can't convert query row " ++ show x

-- | converts SQL data into a Players type
convertPlayers :: [SqlValue] -> Players
convertPlayers [s_id, s_full_name, s_age, s_position, s_team, s_nationality] =
  Players {p_id = fromSql s_id,
           full_name = fromSql s_full_name,
           age = fromSql s_age,
           position = fromSql s_position,
           team = fromSql s_team,
           nationality = fromSql s_nationality}
convertPlayers x = error $ "Can't convert Player data" ++ show x

-- | converts SQL data into a Home_Statistics type
convertHome_Statistics :: [SqlValue] -> Home_Statistics
convertHome_Statistics [s_id, app, goals, goals_90, assists, minutes, clean, conceded] =
  Home_Statistics {player_id = fromSql s_id,
           appearances = fromSql app,
           goals = fromSql goals,
           goals_involved_per_90 = fromSql goals_90,
           assists = fromSql assists,
           minutes_played = fromSql minutes,
           clean_sheets = fromSql clean,
           goals_conceded = fromSql conceded}
convertHome_Statistics x = error $ "Can't convert Home Statistics data " ++ show x
