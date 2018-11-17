{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module XmlLearning.Runner where

import XmlLearning.Cursor
import XmlLearning.Transform ()
import XmlLearning.CreateXml 
import XmlLearning.Hamlet
import XmlLearning.StyleTag

run :: IO ()
run = do
  -- transformDoc
  findTitle
  findTitleWithOperators
  byAttribute
  writeDoc
  createXml
  createXmlWithHamlet
  createXmlWithData
  createWithStyle
