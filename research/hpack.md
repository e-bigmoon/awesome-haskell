- [Quick-reference](https://github.com/sol/hpack#quick-reference)

# github
`github` を指定すると`homepage`, `bug-reports`, `source-repository` を生成してくれる。

```yaml
-- package.yaml
github: user/repo
```

```cabal
-- .cabal
homepage:       https://github.com/user/repo#readme
bug-reports:    https://github.com/user/repo/issues
source-repository head
  type: git
  location: https://github.com/user/repo
```

# 既存プロジェクトから package.yaml を生成する

- [hpack-convert](https://github.com/yamadapc/hpack-convert#readme)
