# Tasty

`Tasty` を使う個人的なメリットとしては

- `Providers` (テスト処理。 `hspec` や `quickcheck` など) と `Ingredients` (テストした結果に対する処理。`terminal` に出力や `html` に出力など) が分離されているため、好きな組み合わせにできる
- `tasty-golden` が良い

## Provider と Ingredient

Provier | 内容 | 調査結果
--------|-------|--------
[tasty-hunit](https://www.stackage.org/package/tasty-hunit) | |
[tasty-golden](https://www.stackage.org/package/tasty-golden) | ゴールデンテスト。ファイルに保存した結果を単体テストする | [example](/research/tasty/tasty-golden.md)
[tasty-smallcheck](https://www.stackage.org/package/tasty-smallcheck) | |
[tasty-quickcheck](https://www.stackage.org/package/tasty-quickcheck) | |
[tasty-hspec](https://www.stackage.org/package/tasty-hspec) | |
[tasty-program](https://www.stackage.org/package/tasty-program) | |

Ingredient | 内容 | 調査結果
--------|-------|--------
[tasty-ant-xml](https://www.stackage.org/package/tasty-ant-xml) | |
[tasty-rerun](https://www.stackage.org/package/tasty-rerun) | |
[tasty-html](https://www.stackage.org/package/tasty-html) | テスト結果を HTML で出力する | [example](/research/tasty/tasty-html.md)
[tasty-stats](https://www.stackage.org/package/tasty-stats) | |

以下、README.md の一部抜粋。

**Tasty** は Haskell におけるモダンテストフレームワークです。

単体テスト、黄金テスト、QuickCheck/SmallCheck プロパティや、その他のテストを組み合わせて、単一のテストスイートを定義できます。

機能

- 並列にテストを実行しますが、結果はいつも同じ順番で出力されます。
- コマンドラインからパターンを指定することで、テストをフィルタリングできます。
- テスト結果は階層的に色付きで出力されます。
- テスト統計をレポートします
- ソケット、一時ファイルなどの確保・解放を各テストで共有できます。
- 拡張性があります。add your own test providers and ingredients (runners) above and beyond those provided

## Example
### コード
```haskell
-- readme-example.hs
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
```

### 実行結果

```bash
$ stack script readme-example.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Properties
    (checked by SmallCheck)
      sort == sort . reverse:           OK (0.14s)
        1333 tests completed
      Fermat's little theorem:          OK
        11 tests completed
      Fermat's last theorem:            FAIL
        there exist 0 0 0 3 such that
          condition is false
    (checked by QuickCheck)
      sort == sort . reverse:           OK (0.04s)
        +++ OK, passed 100 tests.
      Fermat's little theorem:          OK
        +++ OK, passed 100 tests.
      Fermat's last theorem:            OK
        +++ OK, passed 100 tests.
  Unit tests
    List comparison (different length): OK
    List comparison (same length):      FAIL
      expected: LT
       but got: GT

2 out of 8 tests failed (0.19s)
```

## 参考

- [tasty on github](https://github.com/feuerbach/tasty)
- [tasty on stackage](https://www.stackage.org/package/tasty)
