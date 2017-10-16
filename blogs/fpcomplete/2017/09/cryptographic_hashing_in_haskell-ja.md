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