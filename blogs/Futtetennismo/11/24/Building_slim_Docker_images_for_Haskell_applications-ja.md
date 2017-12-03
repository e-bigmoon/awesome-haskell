# Haskell のアプリケーション向けに軽量の Dockerイメージ を作る
アプリケーションを [Docker の実行可能イメージ](https://www.infoq.com/articles/docker-executable-images) として提供するのは、アプリケーションを配布するのに手頃な方法です。ツールチェインやフレームワーク、依存関係をインストールする必要が無くなります。ただ Dockerイメージをプルして走らせればいい。本当に簡単なことです。しかし Dockerイメージは、サイズ的に尋常じゃなく大きくなる可能性があります。アプリケーションを走らせるのに必要な、全ての依存関係をインストールする必要があるからです。ユーザとしてはかなりイライラしますよね。かなり限定された問題を解く小さなアプリケーションを使いたい時に、2GB の Dockerイメージをダウンロードしなければならない状況を考えて見てください! 嫌ですよね。そして実際、そんなことをする必要はありません。コンパクトな Dockerイメージに、実行可能ファイルだけ入れて配布したらどうでしょうか? アプリケーションが Haskell でビルドされていた場合、どうやったらいいのでしょうか?

私は　ちょっと前、[pet-project](https://github.com/futtetennista/ServerlessValidator) の開発をしていた時、この問題に直面しました。Dockerイメージはもう少しで 2GB になるところでしたが、アプリケーションがやっていたのはただ 1つ。YAMLファイルのバリデーションです。2、3日前まで良い解決策が見つからなかったのですが、[feram.io](https://feram.io/) のみなさんが、[このブログ記事](https://blog.alexellis.io/mutli-stage-docker-builds/)を教えてくれました (ありがとうみなさん!)。マルチステージビルド...だと!? こんなことができるなんて全然知りませんでした! なので私は pet-project を見直して、これを Haskell でどうやったら実現できるのか見て見ました (ブログ記事のアプリケーションは Go で書かれているので)。辛くて時間のかかる実験と失敗の結果、やっとたどり着いたやり方は、その記事ので説明されていたものより少し面倒なものになってしまいましたが、やる価値はあります。最終的に、Dockerイメージは、**2GB から 17.1MB - 5MB のサイズ**になりました! 最初は単純なマルチステージビルドをやったのですが、問題がありまして。全ての Haskell の依存関係をコンパイルしないといけないので、最初のマルチステージビルドは完了するまでに時間がかかり、2回目はほんの数秒で終わります。こういう理由があって、イメージを　2つに分けることにしました。要するに、ブログ記事の builder pattern をあえて使いました。まずは必要な Haskell の依存関係全てをコンパイルしたベースイメージをビルドします。そして、マルチステージビルドを使って実行可能イメージを作ります。ベースイメージの `Dockerfile` はそんなに面白いものではないです:

```Dockerfile
# Dockerfile.builder
# docker build -t futtetennista/serverless-validator-builder --file Dockerfile.builder .
FROM haskell:8.0

# Install dependecies needed to compile Haskell libraries
RUN apt-get update && apt-get install --yes \
    xz-utils \
    make

RUN stack --resolver lts-9.14 install base \
    protolude \
    text \
    aeson \
    yaml \
    unordered-containers \
    case-insensitive \
    regex-compat
```

Linux の依存関係をインストールして、Haskell の依存関係をビルドしているだけですね。実行可能イメージのものはもう少し面白いですよ:

```Dockerfile
# Dockerfile
# docker build -t futtetennista/serverless-validator .
FROM futtetennista/serverless-validator-builder as builder

WORKDIR "/home/serverless-validator/"

# copy the contents of the current directory in the working directory
COPY . .

RUN stack --resolver lts-9.14 install && \
    strip /root/.local/bin/serverless-validator


FROM fpco/haskell-scratch:integer-gmp

COPY --from=builder /root/.local/bin/serverless-validator /bin/

ENTRYPOINT ["/bin/serverless-validator"]
```

まず、ベースコンテナで実行可能ファイルのコンパイル、リンクをして、以下の `man strip` の説明のように、望ましくないデータを取り除きます:

> strip removes or modifies the symbol table attached to the output of the assembler and link editor. This is useful to save space after a program has been debugged and to limit dynamically bound symbols.

そして最後に、ベースコンテナから実行可能コンテナに、実行可能ファイルをコピーします。[fpco/haskell-scratch](https://hub.docker.com/r/fpco/haskell-scratch/) という Dockerイメージは、私の個人的なヒーロー、[Michael Snoyberg](https://twitter.com/snoyberg)さんが作ったもので、[このブログ記事](https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb)でしばらく前に紹介されています。これは最小の Linuxイメージで (2MB 程度の)、
Haskell のアプリケーションを動かすベースイメージとして使うことができます。2年間更新されていませんが、今でも完璧に動きます ([snoyberg/haskell-scratch](https://hub.docker.com/r/snoyberg/haskell-scratch/) というタグがついたまた別の Dockerイメージがありますが、これは非推奨だと思います。)。

もう一度マイケルと FP Complete の人たちに、Haskellerたちが日々のコーディングで遭遇する、現実的な多くの問題を解決してくれることを感謝したいと思います!

## 追記
このテクニックは、自分のアプリケーションが元からあるフレームワークやライブラリをビルドしなければいけない時は、いつでも使えます。例えばまさにこのサイトです! こいつをキャッシュなしで必要な依存関係全てをコンパイルした結果、大体 14分かかりました。キャッシュありで 1分 32秒。コンパイル済みの依存関係を持っているベースイメージを使うと 1分 18秒でした。
