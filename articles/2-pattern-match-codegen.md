# title

パターンマッチのコード生成について

# uid

2

# tags

ocaml compiler

# updatedAt

2020-08-05 21:40

# body

## 何の話？

[tsukimizake/hscaml](https://github.com/tsukimizake/hscaml) でocamlサブセットのコンパイラを作っている。
現状パーサと基本的なHM型推論が動いており、今はlet多相の型チェックを作っている。

まだコード生成には手を付けていないのだが、パターンマッチはどのようなコードに落とせるのかなんとなく気になったので考えた実装方針が以下。

## 最も単純な例

まずこのような型とそれに対するマッチを考える。

```ocaml
type foo = Hoge | Huga

let f x = match (x:foo) with
         | Hoge -> 'a'
         | Huga -> 'b';;
```

これを機械語に落とすにはどのようにすれば良いだろうか？
まずコンパイル時に値コンストラクタにidを振り、Hogeに0, Hugaに1を対応させる。
すると、foo型の変数が実行時に持つ情報としては0,1のタグだけにできるので、C言語で言う以下に帰着できる。

```c
typedef int foo;

char f(foo x) {
  if (x == 0) {
    return 'a';
  } else if (x == 1) {
    return 'b';
  }
}
```

引数がない場合は簡単に扱えそうだということがわかる。

## 引数がある場合

```ocaml
type foo = Hoge of int * int | Huga of int

let f x = match (x:foo) with
         | Hoge (a, b) -> a + b
         | Huga a -> a;;
```

Hogeはintを2つ持ち、Hugaはintを一つ持っている。

値コンストラクタによって引数の数も型も変わるが、スタック上に乗せるものは固定長でないと困る。こういう時はポインタを使うと相場が決まっている。  

スタックにはこれまでと同様の値コンストラクタのidと、引数リストへのポインタを置く。

このポインタの参照先には、値コンストラクタからもっとも引数リストのバイト数が長いものの長さだけ引数リストのバッファをヒープに確保しておく。

この場合int * intのほうがintより長いので、2ワードを確保する。

(補足:intは4バイトで済むので64bitOSではメモリを余分に使うコードになっているが、次項で再帰的なデータ構造を扱う際に同じ場所にポインタを置くことになるので少し贅沢に1変数あたり1ワード使う)

```ocaml
let x = Hoge(1,2);;
```

とすると、実行時にxが持つのは値コンストラクタHogeに対応するidの0と、変数2つぶんmallocされた配列へのポインタとなる。
配列内には1,2が入る。

```ocaml
let y = Huga(3);;
```

とすると、実行時にyが持つのは値コンストラクタHugaに対応するidの1と、変数2つぶんmallocされた配列へのポインタとなる。

配列内には3だけが入り、2要素目には何も入れない。(flow graphの解析によってHogeが入り得ないとわかった場合はひとつ分の配列で済むかもしれないが、今は考えない。)


結果として、Cでは以下のようなコードになるだろうか。

```c
#include <stdio.h>
#include <stdlib.h>

struct foo {
  int tag;
  void **args;
};

int f(struct foo *x) {
  if (x->tag == 0) {
    return (int)(x->args[0]) + (int)(x->args[1]);
  } else if (x->tag == 1) {
    return ((int *)x->args)[0];
  }
  printf("unknown tag %d\n", x->tag);
  exit(1);
}

int main(void) {

  // let x = Hoge(1,2);;
  void **p = malloc(2 * sizeof(void *));
  struct foo *x = (struct foo *)malloc(sizeof(struct foo));
  x->tag = 0;
  x->args = p;
  p[0] = (void *)1;
  p[1] = (void *)2;

  // let y = Huga(3);;
  void **q = malloc(2 * sizeof(void *));
  struct foo *y = (struct foo *)malloc(sizeof(struct foo));
  y->tag = 1;
  q[0] = (void *)3;
  y->args = q;

  // f x
  printf("%d\n", f(x)); // => 3

  // f y
  printf("%d\n", f(y)); // => 3
}
```

## 再帰的なデータ構造を扱う例
最後に再帰的なデータ構造の例として、cons listもこのメモリモデルで行けるかを考えてみる。
以下のコードを考える。

```ocaml
type list = Cons of int * list | Nil;;
let x = Cons(1, Cons(2, Nil));;

let rec sum xs = match xs with
 | Nil -> 0
 | Cons(a, ys) -> a + sum ys;;
```

型引数にlistが出てきた場合はやはり中身のサイズがわからないのでポインタで参照することになる。


NilはNilのid1のみを持つ。

Cons(2, Nil)はConsのid0と2ワードのバッファへのポインタを持つ。バッファにはintの2とNilへのポインタが入る。

Cons(1, Cons(2,Nil))はConsのid0と2ワードのバッファへのポインタを持つ。バッファにはintの1とCons(2, Nil)へのポインタが入る。

以下のようなコードになるだろうか。

```c
#include <stdio.h>
#include <stdlib.h>

// type list = Cons of int * list | Nil;; に対応する
struct list {

  // Consなら0, Nilなら1になる。パターンマッチ時にはまずこれを読む
  int tag;

  // 引数が入る。Cons of int * listなら2ワード
  // 先頭1ワードがint,続く1ワードがlistへのポインタ
  void **args;
};

int sum(struct list *xs) {
  if (xs->tag == 0) {
    struct list *cdr = xs->args[1];
    return ((int *)xs->args)[0] + sum(cdr);
  } else if (xs->tag == 1) {
    return 0;
  }
  printf("unknown tag: %d\n", xs->tag);
  exit(1);
}

int main(void) {

  // Nil
  void **p = malloc(2 * sizeof(void *));
  a->tag = 1;

  // Cons(2, Nil);
  void **q = malloc(2 * sizeof(void *));
  struct list *b = (struct list *)malloc(sizeof(struct list));
  b->tag = 0;
  b->args = q;
  q[0] = (void *)2;
  q[1] = a;

  // Cons(1, Cons(2, Nil))
  void **r = malloc(2 * sizeof(void *));
  struct list *x = (struct list *)malloc(sizeof(struct list));
  x->tag = 0;
  x->args = r;
  r[0] = (void *)1;
  r[1] = b;

  printf("%d\n", sum(x)); // => 3
}
```

## とりあえずのまとめ
久々にCを書いたらものすごくつらかった
