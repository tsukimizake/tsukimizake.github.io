{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Time.Calendar as Day
import qualified Data.Time.LocalTime as Time
import qualified System.IO as IO
import qualified Data.Fixed as F

template :: String -> Time.LocalTime -> String
template title time = "#title\n" 
  ++ title
  ++ "\n#tags\n\n"
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
   where 
     stripSeq :: F.Pico -> F.Pico
     stripSeq x = F.MkFixed $ floor x
