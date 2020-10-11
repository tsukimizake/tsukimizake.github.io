{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Fixed as F
import qualified Data.List as L
import qualified Data.Time.Calendar as Day
import qualified Data.Time.LocalTime as Time
import qualified System.Directory as Dir
import qualified System.IO as IO

template :: String -> Int -> Time.LocalTime -> String
template title uid time =
  "# title\n"
    ++ title
    ++ "\n# uid\n"
    ++ show uid
    ++ "\n# tags\n\n"
    ++ "#updatedAt\n"
    ++ showTime time
    ++ "\n# body\n\n"

--  drop seconds 2020-08-04 15:10:0223884 => 2020-08-04 15:10
showTime :: Time.LocalTime -> String
showTime =
  reverse . drop 1 . dropWhile (/= ':') . reverse . show

findValidUid :: IO Int
findValidUid = do
  files <- Dir.listDirectory "../articles/"
  let ids = map (read . takeWhile (/= '-')) . filter ((/= '.') . head) $ files
  return . head $ [0 ..] L.\\ ids

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
