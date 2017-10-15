- [github](https://github.com/feuerbach/tasty)

# Tasty

**Tasty** は Haskell におけるモダンテストフレームワークです。

単体テスト、黄金テスト、QuickCheck/SmallCheck プロパティや、その他のテストを組み合わせて、単一のテストスイートを定義できます。

機能

- 並列にテストを実行しますが、結果はいつも同じ順番で出力されます。
- コマンドラインからパターンを指定することで、テストをフィルタリングできます。
- テスト結果は階層的に色付きで出力されます。
- テスト統計をレポートします
- ソケット、一時ファイルなどの確保・解放を各テストで共有できます。
- 拡張性があります。add your own test providers and ingredients (runners) above and beyond those provided

## golden-test
- [github](https://github.com/feuerbach/tasty-golden)
- [stackage](https://www.stackage.org/package/tasty-golden)

ファイルの入出力などをともなうテストのことをゴールデンテストと言うらしい。
