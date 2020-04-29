module Main where

import qualified Data.Aeson as J
import Data.Either
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import Model
import Parser
import qualified System.Directory as Dir
import System.Exit
import qualified System.IO as IO
import qualified Text.Parsec as P

archieveArticles :: [String] -> Either [P.ParseError] J.Value
archieveArticles contents = do
  let parseResults :: ([Either P.ParseError Article]) = map (parseContents . T.pack) contents
  let errs = lefts parseResults
  if not $ null errs 
    then Left errs
    else do 
      let articles = rights parseResults
      Right $ J.toJSON articles


endsWith :: String -> String -> Bool
endsWith short long = M.isJust $ L.stripPrefix (reverse short) (reverse long)

main :: IO ()
main = do
  files <- Dir.listDirectory "../articles"
  let mds = map ("../articles/" ++) $ filter (endsWith ".md") files
  contents :: [String] <- mapM IO.readFile mds
  let articles = archieveArticles contents
  case articles of 
    Right res -> do 
      print res
    Left err -> print err

