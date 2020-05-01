module Main where

import qualified Data.Time.Calendar as Day
import qualified Data.Time.LocalTime as Time
import qualified System.IO as IO

template :: String -> Time.LocalTime -> String
template title time = "#title\n" 
  ++ title
  ++ "#tags\n\n"
  ++ "#updatedAt\n"
  ++ show time
  ++ "\n#body\n"

main :: IO ()
main = do
  putStr "insert title: "
  IO.hFlush IO.stdout 
  title <- getLine 
  time <- Time.zonedTimeToLocalTime <$> Time.getZonedTime 
  writeFile ("../articles/" ++ title ++ ".md") (template title time)
