{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module XmlLearning.Hamlet where

import Data.Map (empty)
import Prelude hiding (writeFile)
import Text.XML
import Text.Hamlet.XML
import Data.Text (Text, pack)

createXml :: IO ()
createXml =
  writeFile def "resources/test4.xml" $ Document (Prologue [] Nothing []) root []
    where
      root = Element "html" empty
          [ NodeElement $ Element "head" empty
            [ NodeElement $ Element "title" empty
              [ NodeContent "My"
              , NodeElement $ Element "b" empty
                [ NodeContent "Title" ]
              ]
            ]
          , NodeElement $ Element "body" empty
            [ NodeElement $ Element "p" empty
              [ NodeContent "foo bar baz" ]
            ]
          ]

-- This is much more readable
createXmlWithHamlet :: IO ()
createXmlWithHamlet = do
  writeFile def "resources/test5.xml" $ Document (Prologue [] Nothing []) root []
    where
      root = Element "html" empty [xml|
<head>
  <title>
    My #
      <b>Title
  <body>
    <p>foo bar baz
|]

data Person = Person
  { personName :: Text
  , personAge :: Int }

people :: [Person]
people = [ Person "Michael" 26
         , Person "Miriam" 25
         , Person "Eliezer" 3
         , Person "Gavriella" 1 ]

createXmlWithData :: IO ()
createXmlWithData = do
  writeFile def "resources/people.xml" $ Document (Prologue [] Nothing []) root []
    where
      root = Element "html" empty [xml|
<head>
  <title>Some People
<body>
  <h1>Some People
  $if null people
      <p>There are no people.
  $else
      <dl>
          $forall person <- people
              ^{personNodes person}
|]

personNodes :: Person -> [Node]
personNodes person = [xml|
<dt>#{personName person}
<dd>#{pack $ show $ personAge person}
|]
