module Model where

import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import GHC.Generics

type Tag = T.Text

data Article = Article
  { title :: T.Text,
    uid :: Int,
    updatedTime ::
      Integer,
    tags :: [Tag],
    articleText :: T.Text
  }
  deriving (Show, Generic)

instance J.ToJSON Article

