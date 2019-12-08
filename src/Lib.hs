module Lib
    ( batchInsert,
      ageQuery,
      joinQuery
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad(when)
import Types
import System.Random(randomRIO)

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
       --commit conn

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
        --commit conn

insertPlayersIOStmt :: Connection -> IO Statement
insertPlayersIOStmt conn = prepare conn "INSERT INTO Players VALUES (?,?,?,?,?,?)"

insertStatisticsIOStmt :: Connection -> IO Statement
insertStatisticsIOStmt conn = prepare conn "INSERT INTO Home_Statistics VALUES (?,?,?,?,?,?,?,?)"

batchInsert :: String -> IO ()
batchInsert football_data = do
  conn <- connectSqlite3 "football_data3.db"
  createPlayers conn
  createStatistics conn
  let format_data = tail $ lines football_data

  let players = map csvLineToPlayers format_data  -- takes each line, breaks down
  stmt <- insertPlayersIOStmt conn -- prepare insert statements, this is for players
  executeMany stmt $ map playersToSqlValues players -- mapp insert statements to players

  let home_statistics = map csvLineToStatistics format_data
  stmt2 <- insertStatisticsIOStmt conn
  executeMany stmt2 $ map home_StatisticsToSqlValues home_statistics
  commit conn

------- NEW CODE, QUERIES ------------------------------------------------

ageQuery :: IO ()
ageQuery = do
   conn <- connectSqlite3 "football_data3.db" -- Connect to the database
   res <- quickQuery' conn
            "SELECT * from Players where age > 45" [] -- Run the query and store the results in r
   print (map convertAgeQuery res)
   --let stringRows = map convRow r -- Convert each row into a String
   --mapM_ putStrLn stringRows -- Print the rows out
   disconnect conn -- And disconnect from the database

convertAgeQuery :: [SqlValue] -> Players
convertAgeQuery [s_id, s_full_name, s_age, s_position, s_team, s_nationality] =
  Players {p_id = fromSql s_id,
           full_name = fromSql s_full_name,
           age = fromSql s_age,
           position = fromSql s_position,
           team = fromSql s_team,
           nationality = fromSql s_nationality}
convertAgeQuery x = error $ "Can't convert query row " ++ show x

joinQuery :: IO ()
joinQuery = do
   conn <- connectSqlite3 "football_data3.db"
   res <- quickQuery' conn
            "SELECT Players.full_name, Players.position, Players.team, Home_Statistics.appearances \
            \ FROM Players INNER JOIN Home_Statistics ON Players.p_id=Home_Statistics.player_id \
            \ WHERE Home_Statistics.appearances = 7" []
   print (map convertJoinQuery res)
   disconnect conn

-- converts each row into String values
convertJoinQuery :: [SqlValue] -> String
convertJoinQuery [s_full_name, s_position, s_team, h_app] =
   "{Name: "++ ((fromSql s_full_name) :: String) ++
   ", Position: " ++ ((fromSql s_position) :: String) ++
   ", Team: " ++((fromSql s_team) :: String) ++
   ", Appearances: " ++((fromSql h_app) :: String) ++ "}"
convertJoinQuery x = error $ "Can't convert query row " ++ show x


{--
    where convRow :: [SqlValue] -> String
          convRow [sqlId, sqlDesc] =
              show intid ++ ": " ++ desc
              where intid = (fromSql sqlId)::Integer
                    desc = case fromSql sqlDesc of
                             Just x -> x
                             Nothing -> "NULL"
          convRow x = fail $ "Unexpected result: " ++ show x --}
{--
--getFootballData :: IConnection conn => conn -> IO [Podcast]
getPlayers :: Connection
getPlayers dbh =
    do res <- quickQuery' dbh
              "SELECT * FROM Players ORDER BY p_id" []
       return (map convPodcastRow res)

{- | Convert the result of a SELECT into a Podcast record -}
--}


{------
  let formatFunc = [csvLineToPlayers, csvLineToStatistics]
  let insertFunc = [insertPlayersIOStmt, insertStatisticsIOStmt]
  let convertFunc = [playersToSqlValues, home_StatisticsToSqlValues]

  applyFunctions formatFunc insertFunc convertFunc formatdata
  commit conn

applyFunctions x y z _ = x y z
applyFunctions (x:xs) (y:ys) (z:zs) formatdata =
  let formatted_data = map (xs applyFunctions) formatdata
  stmt <- (ys applyFunctions) conn
  executeMany stmt $ map (zs applyFunctions) formatdata
------}

--getID :: IO Int
-- getID = do
--   value <- randomRIO (1, 90000)
--   return $ purefun value
--
-- purefun x = x
{--
  line 68: let players = map csvLineToPlayers $ tail $ lines football_data
we user tail since the first line of the csv file is the field names.

need to make primary key in batch as it must be the same for both tables
--}

{--
initialiseDB :: Connection -> IO ()
initialiseDB conn = do
     tables <- getTables conn
     --when (not ("Players" `elem` tables)) $
        --  do
            run conn
                "CREATE TABLE Players (\
                \p_id VARCHAR(500) PRIMARY, \
                \full_name VARCHAR(200) NOT NULL, \
                \age INTEGER DEFAULT NULL, \
                \team VARCHAR(40) DEFAULT NULL, \
                \position VARCHAR(40) DEFAULT NULL, \
                \nationality VARCHAR(40) DEFAULT NULL);\
                \CREATE TABLE Home_Statistics (\
                \player_id VARCHAR(500), \
                \appearances INTEGER DEFAULT NULL, \
                \goals INTEGER DEFAULT NULL, \
                \goals_involved_per_90 DOUBLE DEFAULT NULL, \
                \assists INTEGER DEFAULT NULL, \
                \minutes_played INTEGER DEFAULT NULL, \
                \clean_sheets INTEGER DEFAULT NULL, \
                \goals_conceded INTEGER DEFAULT NULL, \
                \FOREIGN KEY (player_id) REFERENCES Players(p_id))" [];
               return ()
   when (not ("Home_Statistics" `elem` tables)) $
          do run conn
                "CREATE TABLE Home_Statistics (\
                \player_id VARCHAR(500), \
                \appearances INTEGER DEFAULT NULL, \
                \goals INTEGER DEFAULT NULL, \
                \goals_involved_per_90 DOUBLE DEFAULT NULL, \
                \assists INTEGER DEFAULT NULL, \
                \minutes_played INTEGER DEFAULT NULL, \
                \clean_sheets INTEGER DEFAULT NULL, \
                \goals_conceded INTEGER DEFAULT NULL, \
                \FOREIGN KEY (player_id) REFERENCES Players(p_id))" []
              return ()
    commit conn
--}
