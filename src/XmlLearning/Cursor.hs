{-# LANGUAGE OverloadedStrings #-}
module XmlLearning.Cursor where

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

findTitle :: IO ()
findTitle = do
  doc <- readFile def "resources/test.xml"
  let cursor = fromDocument doc
  print $ T.concat $
          child cursor >>= element "head" >>= child
                       >>= element "title" >>= descendant >>= content

findTitleWithOperators :: IO ()
findTitleWithOperators = do
  doc <- readFile def "resources/test.xml"
  let cursor = fromDocument doc
  print $ T.concat $
    cursor $/ element "head" &/ element "title" &// content

byAttribute :: IO ()
byAttribute = do
  doc <- readFile def "resources/test2.xml"
  let cursor = fromDocument doc
  print $ T.concat $ 
    cursor $// element "h2"
           >=> attributeIs "class" "bar"
           >=> precedingSibling
           >=> element "h1"
           &// content
