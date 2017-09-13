--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import           Hakyll hiding (version)

import Data.String (fromString)
import Data.List (intersperse)
import Data.Text (Text, unpack)
import Data.Yaml (FromJSON, decode)
import GHC.Generics (Generic)

data Packages = Packages
  { packages :: [Package]
  } deriving (Generic)

data Package = Package
  { name :: Text
  , version :: Text
  , tags :: [Tag]
  } deriving (Generic)

type Tag = Text

instance FromJSON Packages
instance FromJSON Package

class ToMarkdown a where
  toMarkdown :: a -> Text

instance ToMarkdown Packages where
  toMarkdown (Packages pkgs) = mconcat $ map toMarkdown pkgs

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

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let dpCmd = "mv _site/Readme.md ."
  hakyllWith defaultConfiguration {deployCommand = dpCmd} $ do
    match "pkg-list.yaml" $ do
      route $ constRoute "Readme.md"
      compile $
        getResourceBody >>=
        withItemBody yamlCompiler >>=
        loadAndApplyTemplate "templates/wrapper.md" postCtx
    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

--------------------------------------------------------------------------------
yamlCompiler :: String -> Compiler String
yamlCompiler = return . unpack . maybe "fail" (toMarkdown :: Packages -> Text) . decode . fromString
