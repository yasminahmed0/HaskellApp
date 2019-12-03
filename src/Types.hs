
module Types where

import Database.HDBC --SQL value marshalling, provides fromSql + toSql
import Database.HDBC.Sqlite3

import qualified Data.ByteString as B
import Network.HTTP.Simple

data Players = Players {
   p_id :: String,
   full_name :: String,
   age :: Int,
   position :: String,
   team :: String,
   nationality :: String
} deriving (Show, Eq)

data Home_Statistics = Home_Statistics{
   player_id :: String,
   appearances :: Int,
   goals :: Int,
   goals_involved_per_90 :: Double,
   assists :: Int,
   minutes_played :: Int,
   clean_sheets :: Int,
   goals_conceded :: Int
} deriving (Show, Eq)


playersToSqlValues :: Players -> [SqlValue]
playersToSqlValues (Players p_id full_name age team position nationality)
  = [toSql p_id, toSql full_name, toSql age, toSql team, toSql position, toSql nationality]

sqlValuesToPlayers :: [SqlValue] -> Players
sqlValuesToPlayers [p_id, full_name, age, team, position, nationality]
  = Players (fromSql p_id) (fromSql full_name) (fromSql age) (fromSql team) (fromSql position) (fromSql nationality)

home_StatisticsToSqlValues :: Home_Statistics -> [SqlValue]
home_StatisticsToSqlValues (Home_Statistics player_id appearances goals goals_involved_per_90 assists minutes_played clean_sheets goals_conceded)
  = [toSql player_id, toSql appearances, toSql goals, toSql goals_involved_per_90, toSql assists, toSql minutes_played, toSql clean_sheets, toSql goals_conceded]

sqlValuesToHome_Statistics :: [SqlValue] -> Home_Statistics
sqlValuesToHome_Statistics [player_id, appearances, goals, goals_involved_per_90, assists, minutes_played, clean_sheets, goals_conceded]
  = Home_Statistics (fromSql player_id) (fromSql appearances) (fromSql goals) (fromSql goals_involved_per_90) (fromSql assists) (fromSql minutes_played) (fromSql clean_sheets) (fromSql goals_conceded)

breakAtAllSeparators :: (a -> Bool) -> [a] -> [[a]]
breakAtAllSeparators p [] = []
breakAtAllSeparators p (x:xs) =
  let (fst,rest) =  break p (x:xs)
  in case rest of [] -> [fst]
                  (y:ys) -> fst:(breakAtAllSeparators p ys)

breakCSVLineAtCommas line = breakAtAllSeparators (==',') line

csvLineToPlayers :: String -> Players
csvLineToPlayers line =
  Players (f++t) f (round((read a)::Double)) p t n
    where (f:a:_:_:p:t:_:_:_:n:_) = breakCSVLineAtCommas line

csvLineToStatistics :: String -> Home_Statistics
csvLineToStatistics line =
  Home_Statistics (f++t) (round((read a)::Double)) (round((read g)::Double))
                  ((read i)::Double) (round((read as)::Double)) (round((read m)::Double))
                  (round((read c)::Double)) (round((read gc)::Double))
    where (f:_:_:_:_:t:_:m:_:_:_:a:_:_:g:_:_:as:_:_:_:_:c:_:_:gc:_:_:_:i:_) = breakCSVLineAtCommas line








    
