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
        writeBinaryFile "output/file1" "abc\n"
    , goldenVsFile "goldenVsFile test" "golden/file2.golden" "output/file2" $ do
        writeBinaryFile "output/file2" "abcあいう\n"
    , goldenVsFile "goldenVsFile test (Text)" "golden/file3.golden" "output/file3" $ do
        TIO.writeFile "output/file3" "あいう\n"
    ]
```

`Test.Tasty.Golden` モジュールで定義されている `writeBinaryFile` 関数を使うとバイナリモードで `writeFile` を実行する。

また、ここでは出力結果の見やすさのため、改行を入れている。

### 実行結果

ゴールデンファイルが無い場合は新規でファイルを作成してテストを行ってくれる。

```bash
$ ls golden/
$ ls output/

$ stack script tasty-golden-example.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsFile test:        OK
      Golden file did not exist; created
    goldenVsFile test:        OK
      Golden file did not exist; created
    goldenVsFile test (Text): OK
      Golden file did not exist; created

All 3 tests passed (0.00s)

$ cat golden/file1.golden
abc
$ cat output/file1
abc

$ cat golden/file2.golden
abcBDF
$ cat output/file2
abcBDF

$ cat golden/file3.golden
あいう
$ cat output/file3
あいう
```

ゴールデンファイルが既に存在している場合は、それを使ってテストを行う。

```bash
$ stack script tasty-golden-example.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsFile test:        OK
    goldenVsFile test:        OK
    goldenVsFile test (Text): OK

All 3tests passed (0.00s)
```

## goldenVsString
ゴールデンファイルと出力ファイルの比較ではなく、ゴールデンファイルと文字列 (`ByteString`) の比較

### ソースコード

```haskell
-- tasty-golden-example2.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests =
  testGroup
    "Golden tests"
    [ goldenVsString "goldenVsString test" "golden/file3.golden" $ do return "abc\n"
    , goldenVsString "goldenVsString test" "golden/file4.golden" $ do return "abcあいう\n"
    ]
```

### 実行結果
```bash
$ stack script tasty-golden-example2.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsString test: OK
      Golden file did not exist; created
    goldenVsString test: OK
      Golden file did not exist; created

All 2 tests passed (0.00s)

$ cat golden/file3.golden
abc
$ cat output/file3
cat: output/file3: そのようなファイルやディレクトリはありません

$ cat golden/file4.golden
abcBDF
$ cat output/file4
cat: output/file4: そのようなファイルやディレクトリはありません
```

## goldenVsFileDiff

テストが失敗した際に `diff` を出力してくれる。

### ソースコード

```haskell
-- tasty-golden-example3.hs
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]

goldenTests =
  testGroup
    "Golden tests"
    [ goldenVsFileDiff
        "goldenVsFileDiff test"
        (\ref new -> ["diff", "-u", ref, new])
        "golden/file5.golden"
        "output/file5" $ do writeBinaryFile "output/file5" "abc\n"
    , goldenVsFileDiff
        "goldenVsFileDiff test"
        (\ref new -> ["diff", "-u", ref, new])
        "golden/file6.golden"
        "output/file6" $ do writeBinaryFile "output/file6" "abcあいう\n"
    ]
```

### 実行結果

```bash
$ ls output/
$ ls golden/

$ stack script tasty-golden-example3.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsFileDiff test: diff: golden/file5.golden: そのようなファイルやディレクトリはありません
FAIL
    goldenVsFileDiff test: diff: golden/file6.golden: そのようなファイルやディレクトリはありません
FAIL

2 out of 2 tests failed (0.00s)
```

`diff` を使う場合は比較対象用のファイルが必要となる。

```bash
$ echo abc > golden/file5.golden
$ echo abcあいう > golden/file6.golden

$ stack script tasty-golden-example3.hs --resolver=lts-9.9
Using resolver: lts-9.9 specified on command line
Tests
  Golden tests
    goldenVsFileDiff test: OK
    goldenVsFileDiff test: FAIL
      --- golden/file6.golden	2017-10-16 12:57:12.705983805 +0900
      +++ output/file6	2017-10-16 12:57:15.930175339 +0900
      @@ -1 +1 @@
      -abcあいう
      +abcBDF

1 out of 2 tests failed (0.00s)
```

このように `diff` コマンドの結果が出力される。

## findByExtension

指定したディレクトリ以下に対して、拡張子にマッチしたファイルパスの一覧を返す補助関数も定義されている。

## 参考

- [tasty-golden on github](https://github.com/feuerbach/tasty-golden)
- [tasty-golden on stackage](https://www.stackage.org/package/tasty-golden)
