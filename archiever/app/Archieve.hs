module Main where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import Model
import Parser
import qualified System.Directory as Dir
import qualified System.IO as IO

newArticle :: IO ()
newArticle = do
  undefined

archieveArticles :: IO ()
archieveArticles = do
  files <- Dir.listDirectory "../articles"
  let mds = map ("../articles/" ++) $ filter (endsWith ".md") files
  contents <- mapM IO.readFile mds
  let articles = map (parseContents . T.pack) contents
  return ()
  where
    endsWith :: String -> String -> Bool
    endsWith short long = M.isJust $ L.stripPrefix (reverse short) (reverse long)

main :: IO ()
main = do
  undefined
