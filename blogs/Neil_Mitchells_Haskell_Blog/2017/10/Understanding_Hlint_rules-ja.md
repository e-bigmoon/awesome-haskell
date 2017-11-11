# HLint のルールを理解する

*概要: 新しいバージョンの HLint で foldr を map に変えてヒントを提示するルールを追加したので、それがどうやって動いているのかを説明する。*

[HLint 2.0.10](https://hackage.haskell.org/package/hlint-2.0.10) がリリースされました。これには、本当は `map` を使うべきところで使われている `foldr` を認識するようなルールが含まれています。例:

```haskell
foldr (\curr acc -> (+1) curr : acc) []
```

これは、以下のように書き換えることができます:

```haskell
map (\curr -> (+1) curr)
```

こっちの方がずっと読みやすいですね (続いて、HLint は `map (+1)` を提案します。最初の `foldr` よりもかなり読みやすくなっています)。これを達成するためには、[hlint.yaml](https://github.com/ndmitchell/hlint/blob/master/data/hlint.yaml) に以下のルールを追加する必要がありました。

```yaml
- warn: {lhs: "foldr (\\c a -> x : a) []", rhs: "map (\\c -> x)"}
```

この一文は、`foldr (\c a -> x : a) []` を見たときに、`map (\c -> x)` を警告で提案する、という意味です。次に、マッチする HLint のエンジンは、このテンプレートをプログラム中の全ての部分式に適用します。ここからは、そのときに HLint が取るステップを説明していきます。
