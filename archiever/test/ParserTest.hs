module Main (main) where

import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Time.Calendar as Day
import qualified Data.Time.LocalTime as Time
import Debug.Trace
import Model
import Parser
import Parser
import Test.Hspec
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Text as P

main :: IO ()
main = hspec $ do
  describe "title parser" do
    it "can parse title" do
      let tokens = ["#", "title", "\n", "hoge"] :: [T.Text]
      P.parse titleParser "" (M.mconcat tokens) `shouldBe` Right "hoge"
      P.parse titleParser "" (T.intercalate " " tokens) `shouldBe` Right "hoge"
  describe "tags parser" do
    it "can parse tags" do
      let tokens = ["#", "tags", "\n", "hoge huga"] :: [T.Text]
      P.parse tagsParser "" (M.mconcat tokens) `shouldBe` Right ["hoge", "huga"]
      P.parse tagsParser "" (T.intercalate " " tokens) `shouldBe` Right ["hoge", "huga"]

  describe "updatedAt parser" do
    it "can parse updatedAt" do
      let tokens = ["#", "updatedAt", "\n", "2000-1-1 1:1"] :: [T.Text]
      P.parse updatedAtParser "" (M.mconcat tokens) `shouldBe` Right 946656060000
      P.parse updatedAtParser "" (T.intercalate " " tokens) `shouldBe` Right 946656060000
  describe "body parser" do
    it "can parse body" do
      let tokens = ["#", "body", "\n", "hoge\nhuga\nhoni huwa"] :: [T.Text]
      P.parse bodyParser "" (M.mconcat tokens) `shouldBe` Right "hoge\nhuga\nhoni huwa"
      P.parse bodyParser "" (T.intercalate " " tokens) `shouldBe` Right "hoge\nhuga\nhoni huwa"

    sampleArticle <- runIO $ readFile "./test/sampleArticle.md"
    it "can parse whole post" do
      case parseContents $ T.pack sampleArticle of
        Right x  -> do
          title x `shouldBe` "honi"
          tags x `shouldBe` ["test"]
          updatedTime x `shouldBe` 1587615600000
          articleText x `shouldBe` "ほにほに！ふわっ！ふわっ！ ٩(*╹ω╹*)و！！\n\n"
        Left err -> do 
          print err
          1 `shouldBe` 0
              
