# title
定数たたみ込みともちょっと違うし何の話なんだろうこれ

# uid
4

# tags
compiler

#updatedAt
2021-01-06 23:08

# isDraft 
True

# body
```hs
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad
import Data.IORef
import System.IO.Unsafe

data Expr = Expr :+ Expr | Literal Int deriving (Show)

data Operation = Add Operation Operation | Init String Operation | Var String | Const Int deriving (Show)

type Env = Char

env :: IORef Env
env = unsafePerformIO $ newIORef 'a'

gensym :: IO String
gensym = do
  x <- readIORef env
  modifyIORef' env succ
  pure [x]

breakExpr :: String -> Expr -> IO [Operation]
breakExpr retsym (Literal n) = pure [Init retsym $ Const n]
breakExpr retsym (l :+ r) =
  do
    l' <- gensym
    le <- breakExpr l' l
    r' <- gensym
    re <- breakExpr r' r
    pure $ join [le, re, [Init retsym $ Add (Var l') (Var r')]]

main :: IO ()
main = do
  print =<< breakExpr "res" (Literal 1 :+ Literal 2)
  print =<< breakExpr "res" (Literal 1 :+ (Literal 2 :+ Literal 3))
  print =<< breakExpr "res" ((Literal 1 :+ Literal 2) :+ Literal 3)
```
まあこんな感じの足し算の式をSSA方式にするやつを考えよう。こいつの出力はこんな感じになる。
```
[Init "a" (Const 1),Init "b" (Const 2),Init "res" (Add (Var "a") (Var "b"))]
[Init "c" (Const 1),Init "e" (Const 2),Init "f" (Const 3),Init "d" (Add (Var "e") (Var "f")),Init "res" (Add (Var "c") (Var "
d"))]
[Init "h" (Const 1),Init "i" (Const 2),Init "g" (Add (Var "h") (Var "i")),Init "j" (Const 3),Init "res" (Add (Var "g") (Var "
j"))]
```
まあなんというか非効率である
ひとつめは1+2をするだけなので[Init "res" (Const 1) (Const 2)]でいい 
ふたつめは1+(2+3)なので、[Init "a" (Add (Const 2) (Const 3)), Init "res" 1 "a"]でいい
みっつめは1+2+3なので[Init "a" (Add (Const 1) (Const 2)), Init "res" "a" 3] でいい

まあそんな感じになってほしいという気持ちを感じてほしい
