{-# LANGUAGE OverloadedStrings #-}
module XmlLearning.CreateXml where

import Data.Map (empty)
import Prelude hiding (writeFile)
import Text.XML

writeDoc :: IO ()
writeDoc =
  writeFile def "resources/test3.xml" $ Document (Prologue [] Nothing []) root []
    where
      root = Element "html" empty
        [ NodeElement $ Element "head" empty
          [ NodeElement $ Element "title" empty
            [ NodeContent "My "
            , NodeElement $ Element "b" empty
              [ NodeContent "Title" ]
            ]
          ]
          , NodeElement $ Element "body" empty
            [ NodeElement $ Element "p" empty
              [ NodeContent "foo bar baz" ]
            ]
        ]
