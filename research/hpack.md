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

# hpack-convert

既存プロジェクトの `.cabal` ファイルから `package.yaml` を生成するためのツール。

- [hpack-convert on github](https://github.com/yamadapc/hpack-convert#readme)
- [hpack-convert on stackage](https://www.stackage.org/package/hpack-convert)

## インストール

### stack でインストール

```bash
$ stack install hpack-convert
```

### ソースコードからインストール

```bash
$ git clone https://github.com/yamadapc/hpack-convert
$ cd hpack-convert
$ stack install
```

## 使い方

```bash
$ cd <project_dir>
$ hpack-convert
``
