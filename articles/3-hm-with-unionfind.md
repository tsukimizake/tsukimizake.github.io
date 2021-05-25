# title
Unionfindを使ってHM型推論を書いた

# uid
3

# tags
compiler

# updatedAt
2020-09-28 19:27

# isDraft 
False 

# body

HM型推論を書くとき教科書通りにASTをトラバースして型変数を振って型変数間の同値関係を集めてその同値類を作るような実装をしていた(伝われ)が、
[Efficient and insightful generation](http://okmij.org/ftp/ML/generalization.html#gen-mismanagement)
を読んでなるほどなあとなり、haskellでsound-eagerのアルゴリズムを実装してみたのが以下。

[tsukimizake/sound-eager](https://github.com/tsukimizake/sound-eager)

型変数が指す型をref型で持つことによって、同じ型を指す型変数を一度にアップデートしている。 
これをhaskellで書くにあたってIORefを使った。型推論を行う部分が全部IOになるのはSTRef使ってST型にしろという話だが、それでもRefの部分をshowしたくなったら`unsafePerformIO . unsafeSTToIO`が必要になりそうとかそもそもhaskellでRefを持ち回って手続き的にやるのはださくないかとか色々言いたくなる。

というわけで、同値類に分けるといえばまず思いつくUnionFindを使って書けないかと思ってわちゃわちゃしたのが以下。

[sound-eager-uf](https://github.com/tsukimizake/sound-eager-uf)

なんというか自明なので書くことがない(終)
