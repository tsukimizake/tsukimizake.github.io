module Main where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import qualified System.Directory as Dir
import qualified System.IO as IO
import Model
import Parser

endsWith :: String -> String -> Bool
endsWith short long = M.isJust $ L.stripPrefix (reverse short) (reverse long)


main :: IO ()
main = do
  files <- Dir.listDirectory "../articles"
  let mds = map ("../articles/" ++) $ filter (endsWith ".md") files
  contents <- mapM IO.readFile mds
  let articles = map (parseContents . T.pack) contents
  return ()
