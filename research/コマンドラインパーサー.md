# optparse-generics

複雑なコマンドラインパーサーじゃない場合はこれで十分。

- [optparse-generic](https://hackage.haskell.org/package/optparse-generic-1.2.2)
- [examples](https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/tree/master/examples)

# 基本的な使い方
```haskell
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveGeneric      #-}
module Options.Generic.Args
  ( Cmd (..)
  , module Options.Generic
  )
  where

import Options.Generic
import Text.Read

data Cmd w = Cmd (w ::: Snapshot <?> "must be nightly or lts")
  deriving Generic

instance ParseRecord (Cmd Wrapped)
deriving instance Show (Cmd Unwrapped)

data Snapshot = Nightly | Lts deriving (Generic)

instance Show Snapshot where
  show Nightly = "nightly"
  show Lts = "lts"

instance Read Snapshot where
  readPrec = do
    Ident s <- lexP
    case s of
      "nightly" -> return Nightly
      "lts" -> return Lts
      _ -> pfail

instance ParseRecord Snapshot
instance ParseFields Snapshot
instance ParseField Snapshot

--
main :: IO ()
main = do
  (opts, help) <- unwrapWithHelp "Test program"
  print (opts :: Cmd Unwrapped)
```

実行例

```bash
>>> stack exec notify -- nightly
Cmd nightly
>>> stack exec notify -- Nightly
Test program

Usage: notify SNAPSHOT

Available options:
  -h,--help                Show this help text
  SNAPSHOT                 must be nightly or lts
```
