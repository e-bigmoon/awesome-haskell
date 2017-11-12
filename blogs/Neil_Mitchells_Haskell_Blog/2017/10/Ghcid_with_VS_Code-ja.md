# VS Code で Ghcid を使う
*概要: 新しいバージョンの Ghcid と VS Code拡張がリリースされ、よりいい感じに動くようになった。*

[Ghcid v0.6.8](https://hackage.haskell.org/package/ghcid) と関連する VS Code拡張
[haskell-ghcid v0.2.0](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid) をリリースしました。
一緒に使うと、Ghcid + VS Code 環境をシンプルなものにしてくれます。

## .ghcid ファイルの読み込み (Ghcid)
Ghcid に、カレントディレクトリに `.ghcid` ファイルがあった場合はそれを追加の引数としてロードする機能が追加されました。
例えば、Shake レポジトリには [.ghcid](https://github.com/ndmitchell/shake/blob/master/.ghcid) ファイルがあって:

```
-c "ghci -fno-code -ferror-spans"
```

`ghcid` に、コマンドでこれを解釈させずに (例えば `.stack-work` があれば `stack` でこれを解釈させずに) 常に `ghci -fno-code -ferror-spans` を
実行させています。このコマンドは、[`.ghci` ファイル](https://github.com/ndmitchell/shake/blob/master/.ghci) があって、必要なファイルを全て
ロードしているのでうまく動いています。ちなみに `-fno-code` はコンパイル速度を上げるために、`-ferror-spans` はエラーのハイライトをより良くするために
指定しています。

## ghcid を開始 (Ghcid VS Code)
VS Code拡張には、`Start Ghcid` という、新しく `ghcid` を開始するアクションが追加されました。開始した後、出力は一時ファイルに保存され、
Problems ペインに表示されます。この拡張は `ghcid` を引数なしで実行するため、`.ghcid` ファイルは注意して慎重に編集してください。

このような変更をしたのは、VS Code からより少ないキーで `ghcid` を開始するためです。以前は専門のフラグやファイルの閲覧、コマンドの実行などが
必要でした。
