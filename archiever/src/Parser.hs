module Parser where

import Data.Fixed
import qualified Data.Text as T
import qualified Data.Time.Calendar as Day
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as PT
import qualified Data.Time.LocalTime as Time
import qualified Foreign.C.Types as C
import Model
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Text as P

sectionParser :: String -> P.Parser a -> P.Parser a
sectionParser sectionName parser = do
  P.string "#"
  P.spaces
  P.string sectionName
  P.spaces
  parser

titleParser :: P.Parser T.Text
titleParser = sectionParser "title" $ T.pack <$> P.many1 P.alphaNum

updatedAtParser :: P.Parser Integer
updatedAtParser = sectionParser "updatedAt" do
  y <- read <$> P.many1 P.digit
  _ <- P.char '-'
  m <- read <$> P.many1 P.digit
  _ <- P.char '-'
  d <- read <$> P.many1 P.digit
  _ <- P.many1 $ P.char ' '
  h <- read <$> P.many1 P.digit
  _ <- P.char ':'
  min <- read <$> P.many1 P.digit
  let localtime = Time.LocalTime (Day.fromGregorian y m d) (Time.TimeOfDay h min 0)
  pure $ (\(MkFixed x) -> x `div` (1000*1000*1000)) $ Clock.nominalDiffTimeToSeconds $ PT.utcTimeToPOSIXSeconds $ Time.localTimeToUTC jst localtime
  where
    jst = Time.TimeZone (9 * 60) False "jst"

tagsParser :: P.Parser [T.Text]
tagsParser = sectionParser "tags" $ map T.pack <$> (P.many1 P.alphaNum `P.sepBy` P.many1 (P.char ' '))

bodyParser :: P.Parser T.Text
bodyParser = sectionParser "body" $ T.pack <$> P.many1 P.anyChar

parseContents :: T.Text -> Either P.ParseError Article
parseContents = P.parse parser ""
  where
    parser :: P.Parser Article
    parser = do
      title <- titleParser
      P.spaces
      tags <- tagsParser
      P.spaces
      updatedTime <- updatedAtParser
      P.spaces
      articleText <- bodyParser
      pure $ Article {..}
