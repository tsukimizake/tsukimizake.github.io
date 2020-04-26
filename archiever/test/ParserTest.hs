module Main (main) where

import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Time.Calendar as Day
import qualified Data.Time.LocalTime as Time
import Debug.Trace
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
      let tokens = ["#", "updatedAt", "\n", "2000/1/1 1:1"] :: [T.Text]
      P.parse updatedAtParser "" (M.mconcat tokens) `shouldBe` Right (Time.LocalTime (Day.fromGregorian 2000 1 1) (Time.TimeOfDay 1 1 0))
      P.parse updatedAtParser "" (T.intercalate " " tokens) `shouldBe` Right (Time.LocalTime (Day.fromGregorian 2000 1 1) (Time.TimeOfDay 1 1 0))
  describe "body parser" do
    it "can parse body" do
      let tokens = ["#", "body", "\n", "hoge"] :: [T.Text]
      P.parse bodyParser "" (M.mconcat tokens) `shouldBe` Right "hoge"
      P.parse bodyParser "" (T.intercalate " " tokens) `shouldBe` Right "hoge"
