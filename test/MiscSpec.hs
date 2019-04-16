{-# LANGUAGE QuasiQuotes #-}
module MiscSpec where

-- this module is testing out various bits that does not need a full spec file just yet

import Test.Hspec

import Data.Text
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet hiding (renderHtml)

data Url = Haskell | Yesod

renderUrl Haskell _ = pack "http://haskell.org"
renderUrl Yesod   _ = pack "http://www.yesodweb.com"

title = pack "This is in scope of the template below"

template :: HtmlUrl Url
template = [hamlet|
<html>
  <head>
    #{title}
  <body>
    <p>
      <a href=@{Haskell}>Haskell
      <a href=@{Yesod}>Yesod
|]

renderedHtml :: String
renderedHtml = "<html>\
               \<head>\
               \This is in scope of the template below\
               \</head>\n\
               \<body>\
               \<p>\
               \<a href=\"http://haskell.org\">Haskell</a>\n\
               \<a href=\"http://www.yesodweb.com\">Yesod</a>\n\
               \</p>\n\
               \</body>\n\
               \</html>\n"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "works with a template" $ do
    it "renders it like HTML" $ do
      let html = template renderUrl
      renderHtml html `shouldBe` renderedHtml
