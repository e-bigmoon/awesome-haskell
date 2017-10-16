# tasty-golden

ファイルの入出力などをともなうテスト (ファイルと比較する) のことをゴールデンテストと言うらしい。

> A golden test is an IO action that writes its result to a file. To pass the test, this output file should be identical to the corresponding «golden» file, which contains the correct result for the test.

# 準備

期待する結果のファイルを用意する。

```bash
$ mkdir output golden
$ echo "abc" > output/file1
$ cat output/file1
abc
```

## 参考

- [tasty-golden on github](https://github.com/feuerbach/tasty-golden)
- [tasty-golden on stackage](https://www.stackage.org/package/tasty-golden)
