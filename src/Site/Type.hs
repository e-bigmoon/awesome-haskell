{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Site.Type where

import GHC.Generics (Generic)

import Site.Class

import Data.Text (Text)
import Data.Yaml (FromJSON)
import Data.List (intersperse)

data Packages = Packages
  { packages :: [Package]
  } deriving (Generic)

instance FromJSON Packages

instance ToMarkdown Packages where
  toMarkdown (Packages pkgs) = mconcat $ map toMarkdown pkgs


data Package = Package
  { name :: Text
  , version :: Text
  , tags :: [Tag]
  } deriving (Generic)

instance FromJSON Package

instance ToMarkdown Package where
  toMarkdown pkg = mconcat $ intersperse " | " row
    where
      row = [ name pkg
            , version pkg
            , createPageDescLink $ name pkg
            , mconcat $ intersperse ", " $ tags pkg
            , "\n"
            ]
      createPageDescLink title = mconcat ["[",title,"](/packages/",title,"/Readme.md)"]



type Tag = Text

