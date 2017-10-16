# tasty-golden

ファイルの入出力などをともなうテスト (ファイルと比較する) のことをゴールデンテストと言うらしい。

> A golden test is an IO action that writes its result to a file. To pass the test, this output file should be identical to the corresponding «golden» file, which contains the correct result for the test.

改行に対する取り扱いには注意する必要がある。推奨はバイナリとしてゴールデンファイルを扱う。

> Note about line endings. The best way to avoid headaches with line endings (when running tests both on UNIX and Windows) is to treat your golden files as binary, even when they are actually textual.

また、バージョン管理システム (`git`) が自動的に改行の変換を行わないように `.gitattributes` に以下の設定を追加しておくことも良い。(ここではゴールデンファイルを `.golden` として保存すると仮定している。

```
*.golden	-text
```

## 準備

ゴールデンファイルとテストの出力結果を保存するディレクトリを事前に作っておく。
ディレクトリが見つからない場合はエラーとなる。

```bash
$ mkdir output golden
```

## goldenVsFile
### ソースコード

```haskell
-- tasty-golden-example.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden

import qualified Data.Text.IO as TIO

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests =
  testGroup
    "Golden tests"
    [ goldenVsFile "goldenVsFile test" "golden/file1.golden" "output/file1" $ do
        writeFile "output/file1" "abc"
    , goldenVsFile "goldenVsFile test (Text)" "golden/file2.golden" "output/file2" $ do
        TIO.writeFile "output/file2" "あいう"
    ]
```

### 実行結果

`golden` ファイルが無い場合は新規でファイルを作成してテストを行ってくれる。

```bash
$ ls golden/
$ ls output/

$ stack script tasty-golden-example.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsFile test:        OK
      Golden file did not exist; created
    goldenVsFile test (Text): OK
      Golden file did not exist; created

All 2 tests passed (0.00s)

$ cat golden/file1.golden
abc $ cat output/file1
abc

$ cat golden/file2.golden
あいう $ cat output/file2
あいう
```

`golden` ファイルが既に存在している場合は、それを使ってテストを行う。

```bash
$ stack script tasty-golden-example.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsFile test:        OK
    goldenVsFile test (Text): OK

All 2 tests passed (0.00s)
```

## 参考

- [tasty-golden on github](https://github.com/feuerbach/tasty-golden)
- [tasty-golden on stackage](https://www.stackage.org/package/tasty-golden)
