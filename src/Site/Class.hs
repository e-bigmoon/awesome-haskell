module Site.Class where

import Data.Text (Text)

class ToMarkdown a where
  toMarkdown :: a -> Text

