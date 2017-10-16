# tasty-html

テスト結果をHTMLとして出力するためのパッケージ

## コード抜粋

`tasty-html` のためのコードは以下の部分。

```haskell
import Test.Tasty.Runners.Html (htmlRunner)

main = defaultMainWithIngredients (htmlRunner:defaultIngredients) tests
```

## コード全体

```haskell
-- tasty-html-example.hs
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Runners.Html (htmlRunner)
import Test.Tasty.SmallCheck as SC

import Data.List

main = defaultMainWithIngredients (htmlRunner : defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps =
  testGroup
    "(checked by SmallCheck)"
    [ SC.testProperty "sort == sort . reverse" $ \list ->
        sort (list :: [Int]) == sort (reverse list)
    , SC.testProperty "Fermat's little theorem" $ \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
  -- the following property does not hold
    , SC.testProperty "Fermat's last theorem" $ \x y z n ->
        (n :: Integer) >= 3 SC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "sort == sort . reverse" $ \list ->
        sort (list :: [Int]) == sort (reverse list)
    , QC.testProperty "Fermat's little theorem" $ \x -> ((x :: Integer) ^ 7 - x) `mod` 7 == 0
  -- the following property does not hold
    , QC.testProperty "Fermat's last theorem" $ \x y z n ->
        (n :: Integer) >= 3 QC.==> x ^ n + y ^ n /= (z ^ n :: Integer)
    ]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $ [1, 2, 3] `compare` [1, 2] @?= GT
  -- the following test does not hold
    , testCase "List comparison (same length)" $ [1, 2, 3] `compare` [1, 2, 2] @?= LT
    ]
```

## 実行結果
以下のように実行すると `result.html` が生成される。

```bash
# スクリプト形式
$ stack script tasty-html-example.hs --resolver=lts-9.9 -- --html result.html
Using resolver: lts-9.9 specified on command line

$ ls
result.html  tasty-html-example.hs

# プロジェクト形式の場合はこのようにして引数を渡す
$ stack test --test-arguments="--html results.html"
```

![result.htmlのスクリーンショット](/research/tasty/tasty-html.png)

マウスでクリックすると、ツリーを開いたり閉じたりできます。

## 参考

- [tasty-htlm on github](https://github.com/feuerbach/tasty-html)
- [tasty-html on stackage](https://www.stackage.org/package/tasty-html)
