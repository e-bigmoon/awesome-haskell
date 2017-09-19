Slack を使うためのパッケージ

パッケージ名 | Hackage | Stackage (lts or nightly) | GitHub | Last Commit
-----------|---------|----------|----------------|--------|
slack-api | [0.12](https://hackage.haskell.org/package/slack-api-0.12) | - | [repo](https://github.com/mpickering/slack-api) | 2017/3/2
slack-notify-haskell | [0.2.3](https://hackage.haskell.org/package/slack-notify-haskell-0.2.3) | - | [repo](https://github.com/tattsun/slack-notify-haskell) | 2015/2/27
danibot | [0.2.0.0](https://hackage.haskell.org/package/danibot-0.2.0.0) | - | [repo](https://github.com/danidiaz/danibot) | 2016/3/19
slack-web | [0.2.0.1](https://hackage.haskell.org/package/slack-web-0.2.0.1) | [nightly](https://www.stackage.org/package/slack-web) | [repo](https://github.com/jpvillaisaza/slack-web) | 2017/8/15
slack | [0.1.0.0](https://hackage.haskell.org/package/slack-0.1.0.0) | - | - | -
linklater | [4.0.0.2](https://hackage.haskell.org/package/linklater-4.0.0.2) | - | [repo](https://github.com/hlian/linklater) | 2017/3/11

これを見る限り候補は `slack-api`, `slack` `linklater` となるが、`slack-web` は `issue` の数がものすごく多いため、まだ開発途中だと思われる。

よって、以下の2つを最終候補とする。

- `slack-api`
- `linklater`

# slack-api
実際に slack-api を使ってみた。

- [stackage-diff](https://github.com/e-bigmoon/stackage-diff)

機能的には特に不便を感じなかったが、ドキュメントが圧倒的に少ないためフロンティア精神が必要である。
`chat_postMessage` の使い方など、戸惑う事が多いが、慣れてしまえば使いやすいライブラリのように思う。

また、今回の利用用途は `slack` へメッセージを投げる事だったが、同様にボットを構築することもできるため、どちらのユースケースにも対応可能である。
