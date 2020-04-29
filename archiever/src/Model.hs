module Model where

import qualified Data.Text as T
import qualified Data.Time.LocalTime as Time
import qualified Data.Aeson as J
import GHC.Generics

type Tag = T.Text

data Article = Article
  { title :: T.Text,
    updatedAt ::
      Time.LocalTime,
    tags :: [Tag],
    body :: T.Text
  }
  deriving (Show, Generic)

instance J.ToJSON Article 
