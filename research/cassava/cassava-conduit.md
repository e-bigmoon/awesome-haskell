# cassava-conduit

`cassava` と `conduit` を組み合わせたライブラリ。巨大な `csv` ファイルを扱う際に利用する。

## サンプルコード

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Conduit (($$), (=$=))
import Data.Conduit.Binary (sinkFile, sourceFile)
import qualified Data.Conduit.Combinators as C
import Data.Csv
       (FromRecord, HasHeader(..), ToRecord, defaultDecodeOptions,
        defaultEncodeOptions)
import Data.Csv.Conduit (CsvParseError, fromCsv, toCsv)

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Either (bimapEitherT, runEitherT)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

import Data.Text (Text)
import GHC.Generics (Generic)

data Person = Person
  { name :: !Text
  , salary :: !Int
  } deriving (Generic)

instance FromRecord Person

instance ToRecord Person

main :: IO ()
main = do
  res <- runEitherT $ bimapEitherT show id $ runResourceT conduitPipeline
  either putStrLn return res

conduitPipeline :: (MonadError CsvParseError m, MonadResource m) => m ()
conduitPipeline =
  sourceFile "sampleinput.csv" $$ fromCsv defaultDecodeOptions NoHeader =$= C.map processInput =$=
  toCsv defaultEncodeOptions =$=
  sinkFile "sampleoutput.csv"

processInput :: Person -> Person
processInput person = person {salary = salary person * 2}
```

## 実行結果

今回入力として用意するのはこういうファイル。

```csv
guchi, 100
```

上記の内容を繰り返して 1.4G のファイルを作った。

```bash
$ du -h sampleinput.csv
1.4G	sampleinput.csv

$ wc -l sampleinput.csv
116728002 sampleinput.csv

$ time stack script csv-file-check.hs --resolver lts-9.9
Using resolver: lts-9.9 specified on command line

real	12m22.748s
user	12m13.952s
sys	0m7.679s


# 別ターミナルで実行
$ top
PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND
31759 user      20   0  1.001t 368524 106548 S  99.7  4.7   4:43.81 ghc
```

## 参考

- [cassava-conduit on github](https://github.com/domdere/cassava-conduit)
- [cassava-conduit on Hackage](https://hackage.haskell.org/package/cassava-conduit)
- [cassava-conduit on Stackage](https://www.stackage.org/package/cassava-conduit)
