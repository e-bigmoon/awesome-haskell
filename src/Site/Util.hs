{-# LANGUAGE OverloadedStrings #-}
module Site.Util where

import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Yaml (decode)

import Site.Type (Packages)
import Site.Class (toMarkdown)

import Hakyll.Core.Compiler (Compiler)

yamlCompiler :: String -> Compiler String
yamlCompiler = return . unpack . maybe "Failure YAML parsing." (toMarkdown :: Packages -> Text) . decode . fromString
