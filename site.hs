{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
import Hakyll

import Site.Util (yamlCompiler)
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
