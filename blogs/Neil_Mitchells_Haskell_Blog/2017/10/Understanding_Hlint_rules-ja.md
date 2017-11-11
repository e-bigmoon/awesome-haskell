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

この一文は、`foldr (\c a -> x : a) []` を見たときに、`map (\c -> x)` を警告で提案する、という意味です。次に、HLint のマッチングエンジンは、このテンプレートをプログラム中の全ての部分式に適用します。ここからは、そのときに HLint が実行するステップを説明していきます。

## ステップ1: 単一化
最初のステップは、`foldr (\c a -> x : a) []` というテンプレートの単一化です。`foldr (\curr acc -> (+1) curr : acc) []` のような、ユーザの部分式に対して行われます。HLint はテンプレート中の 1文字の変数 (`c`, `a`, `x` など) への代入ができる部分を探していき、部分式にマッチさせようとします。この単一化はトップダウンで進行し、マッチしないものが現れた段階で (例えば `foldl` など) その単一化は失敗します。このとき、以下のような変数のバインドの単一化は成功します:

* `c = curr` (ラムダ式の最初の引数)
* `a = acc` (ラムダ式の第2引数)
* `x = (+1) curr` (cons の前)
* `a = acc` (cons の後)

しかし、`foldl (\curr acc -> (+1) curr : acc) []` のような部分式の単一化は失敗します。

## ステップ2: 妥当性のチェック
次のステップは、1回以上バインドしている値が、全ての束縛で等しくなっているかどうかのチェックです。さっきの例では、`a` だけが 2回使われており、常に `acc` にバインドされていました。そのため、この単一化は妥当です。

逆に妥当ではない部分式は `foldr (\curr acc -> (+1) curr : xs) []` のようなものです。

## ステップ3: 置換
バインドが確定したので、それを RHS (等式の右部分) に置換することができます。具体的には、`map (\c -> x)` への変換です。`c` と `x` を例中のバインドで置換します。ちなみに、`a` は RHS の中では登場しないので使いません。置換後は以下のようになります:

```haskell
map (\curr -> (+1) curr)
```

## ステップ4: 自由変数のチェック
`foldr (\curr acc -> f acc : acc) []` という式を考えてみてください。上のルールを適用すれば、`map (\curr -> f acc)` になりますがこれはひどい。なぜなら、局所的にバウンドされた `acc` を、そのスコープにある `acc` だと解釈してしまうからです (もしあれば)。これをどうにかするためには、結果が新しい自由変数を導入してしまわないようにしなければなりません:

```haskell
(freeVars result \\ freeVars hintRuleRHS) `isSubsetOf` freeVars original
```

特に結果に現れる RHS にない (偽の単一化変数は除外) 自由変数は、元の部分式に登場していなければいけません。

これで、`foldr` についての説明は終わりです。これ以外にも、いくつかのケースで使われる別のステップがまだあります。

### ステップA テンプレート中のドットの展開
`map f (map g x) ==> map (f . g) x` というヒントを書いたとすると、HLint は `map f . map g ==> map (f . g)` というルールも導出できることに気づき、それを追加します。なので、HLint のルールをポイントフリーで書くべきではありません。

### ステップB 部分式中のドット / ドルの展開
部分式のマッチの過程で、HLint は　`f $ x` や `(f . g) x` を展開するとマッチが成功する場合、これを展開します。これらの演算子はよく使われているので、関数というよりもかっことして扱われます。

### ステップC スコープのマッチ
修飾名付き関数の単一化を行うとき、HLint はマッチするかどうか調べるために、修飾名の単一化も行います。`import qualified Data.Vector as V` という式があったとき、部分式 `V.length` は `Data.Vector.length` に単一化されます。HLint は完全なインポートの情報を持ち合わせていないので、マッチを推測するためにヒューリスティック的な手法を使います。

### ステップD スコープの移動
ルールの LHS (等式の左部分) のスコープのマッチと同じように、マッチの後、`HLint` は RHS で必要な値の再修飾を行います。例えば `Data.Vector.null` が生成されたとき、`import qualified Data.Vector as V` ということを知っていれば、`V.null` を提案します。

### 全体のコード
全体のコードと関連する部分の定義を知りたいのなら、[the HLint source](https://github.com/ndmitchell/hlint/blob/f4466eed8a8bf6beccfd11052f2e3cfb074f2b44/src/Hint/Match.hs#L100-L114) を見てみてください。`matchIdea` を定義している部分です。ここでは簡単にしたバージョンを示します。スコープの情報とルール (LHS と RHS の)、そして部分式が与えられ、必要に応じて置換後に生成される式を返しているのがわかります。

```haskell
matchIdea :: Scope -> HintRule -> Exp_ -> Maybe Exp_
matchIdea s HintRule{..} original = do
    u <- unifyExp hintRuleLHS original
    u <- validSubst u
    -- need to check free vars before unqualification, but after subst (with e)
    -- need to unqualify before substitution (with res)
    let result = substitute u hintRuleRHS
    guard $ (freeVars result Set.\\ Set.filter (not . isUnifyVar) (freeVars hintRuleRHS))
            `Set.isSubsetOf` freeVars original
        -- check no unexpected new free variables
    return result
```
