module Main where

import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.Text as JT
import Data.Either
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Time.LocalTime as Time
import Model
import Parser
import qualified System.Directory as Dir
import System.Exit
import qualified System.IO as IO
import qualified Text.Parsec as P

parseArticles :: [String] -> [Either P.ParseError Article]
parseArticles contents = do
  map (parseContents . T.pack) contents

endsWith :: String -> String -> Bool
endsWith short long = M.isJust $ L.stripPrefix (reverse short) (reverse long)

main :: IO ()
main = do
  files <- Dir.listDirectory "../articles"
  let mds = map ("../articles/" ++) $ filter (endsWith ".md") files
  contents :: [String] <- mapM IO.readFile mds
  let articles = parseArticles contents
  if all isRight articles
    then do
      let archived = filter (not . isDraft) $ rights articles
      let xs = JT.encodeToLazyText archived
      let distPath = "../articles.json"
      IO.writeFile distPath $ LT.unpack xs
      putStrLn "DONE!"
    else do
      print $ filter (\(_, article) -> isLeft article) $ zip mds articles
