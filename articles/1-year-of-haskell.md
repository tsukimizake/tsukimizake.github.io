#title
haskellを1年仕事で書いてみて思ったこと 

# uid
1

#tags
haskell
#updatedAt
2020-05-02 13:20

#body
この一年間、仕事でServantとPersistentを使ったバックエンドサーバを作っていた。

.hsファイルだけで6万行弱ほどの規模であり、それなりに大規模な開発をhaskellでやる経験を得られた。具体的に知見を書いていく。

## ツールについて

### stack build --fastを使え

これは他でもよく言われているが、stack buildは--fast引数を付けることで最適化を省略してコンパイル時間を短縮することができる。
このプロジェクトのフルビルドだと

```
> time stack build
...
stack build  580.22s user 26.21s system 100% cpu 10:02.25 total

time stack build --fast
...
stack build --fast  134.03s user 17.14s system 104% cpu 2:24.58 total
```

となり、4倍ほど違う。正直stack build --fastでもまだ遅い気はするが……

### hieを信じるな

まずhieは遅い。

特にemacsだとlsp-modeがhieを同期的に呼ぶため、ファイル保存や補完などのたびに数秒固まっていた。私はこれが理由でneovimに移行している。

遅いだけならまだしも不安定で、
何度再起動しても以下のようなエラーを吐いて直らない場合がある。

```
hie-8.6.5: loadObj: /private/var/folders/wc/8z0pjlgn1gz1wgdvptgpnz980000gn/T/ghc86703_0/ghc_571.o: file doesn't exist
2020-05-08 12:49:13.274435 [ThreadId 31] - Scheduler thread exited unexpectedly: loadObj "/private/var/folders/wc/8z0pjlgn1gz1wgdvptgpnz980000gn/T/ghc86703_0/ghc_571.o": failed
```

この場合でも、`rm -rf ~/.cache/hie-bios` してからビルドし直すと動くことがある(100%確実というわけではない)。また、hie-biosが導入される前の0.14.0.0をビルドして使えばこれは起きないようだ。

プロジェクトの規模によってはstack build --fast --file-watchを別画面で走らせながら素のvimで書くのが良いのではないかという気すらしている。

(追記: https://github.com/haskell/haskell-ide-engine/issues/1520 によるとhieではfixされそうにない。haskell-language-serverを使うことを推奨されている)

### フォーマッタについて

フォーマッタとしてfloskellを使用していたが、これは依存しているhaskell-src-extsが更新停止しているため、例えばBlockArgumentsなどの比較的新しい言語拡張に対応していない。

そして、2つほど特定のシチュエーションでひどく遅くなる場合があった。

まず、servantのヘッダの型定義で

```
type ReportHeaders =
    Header "hoge" (HeaderObject Int) 
    ': Header "huga" (HeaderObject Int) 
    ...(同様のものが15個ほど続く)
```

のようなコードをフォーマットしようとするとなぜか死ぬほど遅い(1ファイルに数十分かかる)。

```
type HeaderObjectInt = HeaderObject Int
```

のように定義したtype aliasを渡すというワークアラウンドを行っていた。


また、上ほど致命的な遅さではないが、テストコードなどでdoのネストが深くてひとかたまりが長いコードはフォーマットが遅く、1000行ほどのファイルで30秒~40秒ほどかかっていた。
これはdoの塊を分割することで対応できる。

対応できるとはいえたかがフォーマッタのためにコードを弄るのは本末転倒だ。

実験的にormoluでフォーマットを行ってみたところ上のような問題は特に起こらなかったため、個人的にはormoluを推奨する。

## コーディング規約について
### qualified importを使え
haskellのimportの書き方は色々あるが、実用的には以下の3種だろう。

```
import Hoge

import Hoge (hoge)

import qualified Hoge as H
```

各々の解説は省略する。よく知らない人は https://camlspotter.hatenablog.com/entry/20101212/1292165692 などを読むと良いだろう。

1つめのimport Hogeは先駆者が述べているように読めなくなるのでやるべきではない。IDEが優秀な言語ならなんとでもなるだろうが、上記の通りhieは不安定なので使えないことが多々ある。
我々のプロジェクトでは import Hoge (hoge)スタイルが主に使われていたが、これは
- ソースコード中で Hogeモジュール内のhugaを新しく使う
- import文に戻ってimport Hoge (hoge)をimport Hoge (hoge, huga)に変更する
- 元の位置に戻る

のループを気の狂うような回数やる羽目になるのでおすすめできない。
そんなわけでqualified importを推奨する。

## そのほか
なんか続けて書くかもしれない

候補:

- jsonエンコードを自動生成する話
- typefamilyを使って複数のエンドポイントについて権限制御を1カ所にまとめる話

