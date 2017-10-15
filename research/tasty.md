# Tasty

- [github](https://github.com/feuerbach/tasty)
- [stackage](https://www.stackage.org/package/tasty)

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
### Readme に載っているやつ
`package.yaml` の内容

```yaml
name:                example-tasty
version:             0.1.0.0
#synopsis:
#description:
homepage:            https://github.com/githubuser/example-tasty#readme
license:             BSD3
author:              Author name here
maintainer:          example@example.com
copyright:           2017 Author name here
category:            Web
extra-source-files:
- README.md

dependencies:
  - base >= 4.7 && < 5

executables:
  example-tasty:
    source-dirs:      src
    main:             Main.hs

tests:
  tasty:
    main: src/test.hs
    dependencies:
    - tasty
    - tasty-quickcheck
    - tasty-hunit
    - tasty-smallcheck
```

`src/test.hs` の内容。

```haskell
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

実行すると次のようになる。

```bash
$ stack test
example-tasty-0.1.0.0: test (suite: tasty)

Tests
  Properties
    (checked by SmallCheck)
      sort == sort . reverse:           OK (0.19s)
        1333 tests completed
      Fermat's little theorem:          OK
        11 tests completed
      Fermat's last theorem:            FAIL
        there exist 0 0 0 3 such that
          condition is false
    (checked by QuickCheck)
      sort == sort . reverse:           OK
        +++ OK, passed 100 tests.
      Fermat's little theorem:          OK
        +++ OK, passed 100 tests.
      Fermat's last theorem:            FAIL
        *** Failed! Falsifiable (after 3 tests):
        -2
        2
        0
        3
        Use --quickcheck-replay '3 TFGenR BE66C1757F4A4CFC1E3B5AF61FE24B79B44F7EB2B14B8C6F38B474ECD73AE835 0 255 8 0' to reproduce.
  Unit tests
    List comparison (different length): OK
    List comparison (same length):      FAIL
      expected: LT
       but got: GT

3 out of 8 tests failed (0.20s)

Test suite failure for package example-tasty-0.1.0.0
    tasty:  exited with: ExitFailure 1
Logs printed to console
```

# tasty-html

- [stackage](https://www.stackage.org/package/tasty-html)
- [github](https://github.com/feuerbach/tasty-html)

`package.yaml` の中身

```yaml
name:                example-tasty
version:             0.1.0.0
#synopsis:
#description:
homepage:            https://github.com/githubuser/example-tasty#readme
license:             BSD3
author:              Author name here
maintainer:          example@example.com
copyright:           2017 Author name here
category:            Web
extra-source-files:
- README.md

dependencies:
  - base >= 4.7 && < 5

executables:
  example-tasty:
    source-dirs:      src
    main:             Main.hs

tests:
  tasty:
    main: src/test.hs
    dependencies:
    - tasty
    - tasty-quickcheck
    - tasty-hunit
    - tasty-smallcheck
    - tasty-html
```

`src/test.hs` の中身

```haskell
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html (htmlRunner)

import Data.List

-- main = defaultMain tests
main = defaultMainWithIngredients (htmlRunner:defaultIngredients) tests

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

以下のように実行すると `results.html` が生成される。

```bash
$ stack test --test-arguments="--html results.html"
```

# tasty-golden
- [github](https://github.com/feuerbach/tasty-golden)
- [stackage](https://www.stackage.org/package/tasty-golden)

ファイルの入出力などをともなうテストのことをゴールデンテストと言うらしい。
