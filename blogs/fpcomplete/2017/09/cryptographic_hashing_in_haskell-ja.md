# Haskell で暗号学的ハッシュを扱う
2017年 9月 18日 Michael Snoyman

`cryptonite` は現在、Haskell で暗号を扱う際のデファクトスタンダードです。
一般的に安全なランダムな数字、共通鍵・公開鍵方式、MAC (Message Authentication Code)
等をサポートしていて、その中には今日の話題、暗号学的ハッシュも含まれています。

まず、ハッシュ関数について軽く説明しましょう。ハッシュ関数とは、
任意長のデータを固定長のデータに変換するものです。暗号学的
ハッシュ関数は、暗号を扱う上で望ましい性質をそなえたハッシュ関数
です (詳しくは [Wikipedia](https://en.wikipedia.org/wiki/Cryptographic_hash_function) を参照)。
一般的な暗号学的ハッシュ関数の使われ方の一例として、ダウンロード
されたファイルが改ざんされていないことを保証するための、
チェックサムを提供する、というものがあります。今日使われている
暗号学的ハッシュ関数には、SHA256、Skein512、そしてまぁ、ちょっと
古いですが MD5 などがあります。

`cryptonite` は `[memory](https://www.stackage.org/package/memory)`
というライブラリの一番上の階層にあります。この `memory` ライブラリは、
Byte Array を扱うための型クラスと便利な関数を提供しています。
「全て ByteString でいいのでは?」と思うかもしれませんが、
後ほどこの型クラスの便利さを示します。

一旦これら 2つのライブラリに慣れれば、簡単に使いこなすことができます。
ですが、API のドキュメントを見るだけでは、部分部分がどう組み合わさる
のか理解するのは至難の技です。特に、どこで明示的な型シグネチャが必要に
なるのかの理解が難しい。この記事では、理解するのに必要になるであろう
部分の 1つ 1つの簡単な例を、実行可能なコードで紹介します。
読み進める中で、API のドキュメントについてざっくりと理解していって
ください。

この記事のコードの例は、全て `Stack` のスクリプトインタプリタ機能を
使っています。まず `Stack` をインストールして、次の手順で実行して
ください。

- Main.hs にコードをコピペする
- `stack Main.hs` を実行

## 基本的な型クラス
`String` っぽい型を扱うのは慣れているでしょう? 正格・遅延評価される
`ByteString` や `Text` や普通の `String` などです。正格な
バイトシーケンスを表現する際、`Data.ByteString.ByteString` を思い
浮かべるのではないでしょうか。しかし、これから見るように、
バイトシーケンスとして扱いたい型はいろいろ見つかります。

`memory` はこの要望に答えるべく、以下の 2つの型クラスを定義して
います。

- `ByteArrayAccess` ある型のバイトへ、読み専用のアクセスを提供
- `ByteArray` 読み / 書きのアクセスを提供する。`ByteArrayAcccess` の
  子クラス

例えば、以下に示すコードは、意味もなく `ByteString` と `Byte` の変換を
しています。

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.3 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

main :: IO ()
main = do
  B.writeFile "test.txt" "This is a test"
  byteString <- B.readFile "test.txt"
  let bytes :: BA.Bytes
      bytes = BA.convert byteString
  print bytes
```

`bytestring` ライブラリを使ってファイルの入出力から話を
始めたのは、[入出力は `bytestring` でやるべきだからです]
(http://www.snoyman.com/blog/2016/12/beware-of-readfile)。
`convert`関数を使うと、`ByteString` を `Bytes` に変換する
ことができます。

*練習問題* 上の 2つの型クラスの説明を踏まえて、`convert` の
型シグネチャは何だと思いますか? 答えは[こちら]
(https://www.stackage.org/haddock/lts-9.4/memory-0.14.7/Data-ByteArray.html#v:convert)
。

`bytes` の値につけた、明示的な型シグネチャに気がつきましたか?
えーと、`memory` と `cryptonite` を使っていく上で、これは大事です。
こうして、よく GHC に、型推論についてのヒントを与えてやらないと
いけなくなります。なぜなら、これらのライブラリの中のかなり多くの
関数が、具体的な型ではなく、型クラスを使っているからです。

さて、`ByteArrayAccess` に属する型の例をお見せしましたが、それは
`ByteArray` についての例ではありませんでした。今は型クラスを分ける
意味が分からないかもしれませんが、実際にハッシュを使う段階で、
型クラスを分けることの利点が見えてくると思います。ちょっと待って
くださいね。

### なぜ違う型があるのか
当然、`memory` の中になぜ `Bytes` という型があるのか、疑問に思う人
もいるでしょうね。`ByteString` と同じじゃないの? ってね。
まぁ違うんですけどね。`Bytes` はメモリスライスのオフセットと長さを
追跡しないことで、メモリのオーバーヘッドを小さくしています。
その代わりに、`Bytes` の値をスライスすることは許されません。
言い換えれば、`Bytes` に関する `drop`関数は、バッファの新しいコピーを
作らなければならないということです。

まぁつまり、これはパフォーマンスの問題です。暗号を扱うライブラリは、
一般的にパフォーマンスを重視する必要がありますからね。

また別の `memory` のおもしろい型に、`ScrubbedBytes` というものが
あります。この型は、3つの特別な性質を有しています。`Haddock` によると、

- スコープの範囲から出ればゴシゴシされる
- Showインスタンスは、中身を何 1つとして出力しない
- 定数時間の Eqインスタンス

つまり、これらの性質は何かセンシティブなデータを扱うとき、
ありふれた脆弱性をいくつも塞いでくれるものです。

うん、コードがあまりない説明になりましたね。もっと楽しい
ことをしよう!
