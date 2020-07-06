{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Time.Calendar as Day
import qualified Data.Time.LocalTime as Time
import qualified System.IO as IO
import qualified Data.Fixed as F
import qualified System.Directory as Dir
import qualified Data.List as L

template :: String -> Int -> Time.LocalTime -> String
template title uid time = "#title\n" 
  ++ title
  ++ "\n#uid"
  ++ show uid
  ++ "\n#tags\n\n"
  ++ "#updatedAt\n"
  ++ show time
  ++ "\n#body\n"

findValidUid :: IO Int
findValidUid = do
  files <- Dir.listDirectory "../articles/"
  let ids = map (read . takeWhile (/= '-')) . filter ((/= '.') . head) $ files
  return . head $ [0..] L.\\ ids

main :: IO ()
main = do
  putStr "insert title: "
  IO.hFlush IO.stdout 
  title <- getLine 
  time <- Time.zonedTimeToLocalTime <$> Time.getZonedTime 
  uid <- findValidUid
  writeFile ("../articles/" ++ show uid ++ "-" ++ title ++ ".md") (template title uid time)
   where 
     stripSeq :: F.Pico -> F.Pico
     stripSeq x = F.MkFixed $ floor x
