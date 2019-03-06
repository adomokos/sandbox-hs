{-# LANGUAGE OverloadedStrings #-}
module Lens where

import Control.Lens

import Data.Text
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HM

newtype UserName = UserName Text deriving (Show, Eq)
newtype PetName = PetName Text deriving (Show, Eq)

type Inventory = HM.HashMap Text Item

data User
  = User
  { _userName :: UserName
  , _userScore :: Int
  , _userPet :: Maybe Pet
  , _userInventory :: Inventory
  } deriving (Show, Eq)

data Pet
  = Pet
  { _petName :: PetName } deriving (Show, Eq)

data Item
  = Item
  { _itemValue :: Int
  , _itemWeight :: Int
  } deriving (Show, Eq)

-- Let's write lens

userName :: Lens' User UserName
userName = lens getter setter
  where
    getter user = _userName user
    setter user newName = user { _userName = newName }

score :: Lens' User Int
score = lens _userScore (\user newScore -> user { _userScore = newScore })

-- | Note, that this lens targets a 'Maybe Pet'.
pet :: Lens' User (Maybe Pet)
pet = lens _userPet (\user maybePet -> user {_userPet = maybePet})

-- | Singe letter vars, seriously? Yep.
inventory :: Lens' User Inventory
inventory = lens _userInventory (\u i -> u { _userInventory = i })

----------------------------
petName :: Lens' Pet PetName
petName = lens _petName (\p n -> p { _petName = n })
----------------------------

value :: Lens' Item Int
value = lens _itemValue (\i v -> i { _itemValue = v })

weight :: Lens' Item Int
weight = lens _itemWeight (\i w -> i { _itemWeight = w })

run :: IO ()
run = do
  viewExamples
  composedViewExamples
  previewExamples
  setExamples
  fancySetExamples
  overExamples
  atIxExamples
  toListOfExamples
  hasGotcha
  hasGotchaIx

viewExamples :: IO ()
viewExamples = do
  let bob = User (UserName "Bob") 42 Nothing HM.empty

  print "Bob's name is: "
  print $ view userName bob
  -- or shorter --
  print $ bob ^. userName

  print "Bob's score is: "
  print $ view score bob
  print $ bob ^. score

composedViewExamples :: IO ()
composedViewExamples = do
  let
    bob = User (UserName "bob") 42 Nothing HM.empty
    fitzgerald = Pet (PetName "Fitzgerald")
    jeff = User (UserName "jeff") 42 (Just fitzgerald) HM.empty

  print "Bob's pet's name is: "
  print $ preview (pet . _Just . petName) bob
  print $ bob ^? pet . _Just . petName

  print "Jeff's pet's name is: "
  print $ preview (pet . _Just . petName) jeff
  print $ jeff ^? pet . _Just . petName

previewExamples :: IO ()
previewExamples = do
  let maybeIntA = Just 1
      maybeIntB = Nothing :: Maybe Int

  print "maybeIntA"
  print $ maybeIntA ^? _Just

  print "maybeIntB"
  print $ maybeIntB ^? _Just

  let justiceCity = Just "justice"
      _crashCity = Nothing :: Maybe Text

  print "Unwrap this Maybe Text or die trying!"
  print $ justiceCity ^?! _Just

  print "Crash city!"
  -- print $ crashCity ^?! _Just

setExamples :: IO ()
setExamples = do
  let bob = User (UserName "bob") 0 Nothing HM.empty

  print "Bob, with an updated score"
  print $ set score 42 bob

  print $ (score .~ 43) bob

  -- `&` it's just the reverse application operator
  -- You can read this as: "print bob with score set to 42"
  print $ bob & score .~ 44

fancySetExamples :: IO ()
fancySetExamples = do
  let bob = User (UserName "bob") 0 Nothing HM.empty

  print "Bob changes his name to 'Bill'\
        \, updated his score, and now owns Jeff's pet fish,\
        \who is named Fitzgerald."
  print $
    bob
    & userName .~ (UserName "Bill")
    & score .~ 50
    & pet ?~ (Pet (PetName "Fitzgerald"))

overExamples :: IO ()
overExamples = do
  let fitz = Pet (PetName "Fitz")
  let bob = User (UserName "bob") 0 (Just fitz) HM.empty

  print "Bob scores a point. Way to go, Bob."
  --- These all print bob wit ha score icremented by 1.
  print $ bob & score %~ (\sc -> sc + 1)
  print $ bob & score %~ (+1)
  print $ over score (+1) bob
  print $ bob & score +~ 1

  let bobWithFitzy = bob & pet . _Just . petName %~
        (\(PetName n) -> PetName (T.concat [n, "y"]))
  print $ bobWithFitzy ^? pet . _Just . petName

atIxExamples :: IO ()
atIxExamples = do
  let bob'sInventory = HM.fromList [ ("gold", Item 99 10)
                                   , ("silver", Item 10 9)
                                   ]
      bob = User (UserName "bob") 42 Nothing bob'sInventory

  print "Printing Bob's gold value"
  print $ bob ^? inventory . at "gold" . _Just . value
  print $ bob ^? inventory . ix "gold" . value
  print $ bob ^? inventory . at "doesnotexists" . _Just . value
  print $ bob ^? inventory . ix "doesnotexists" . value

  print "Bob finds a diamond"
  let bobFindsDiamond  = bob & inventory . at "diamond" ?~ (Item 1000 1)
      bobFindsDiamond' = bob & inventory . at "diamond" .~ Just (Item 1000 1)
  print $ bobFindsDiamond ^? inventory . ix "diamond"
  print $ bobFindsDiamond' ^? inventory . ix "diamond"

  print "Bob loses his gold, som points, and is sad"
  let bobLosesGold = bob
        & inventory . at "gold" .~ Nothing
        & score %~ (\sc -> sc - 41)
        & userName .~ UserName "Sad Bob"

  print $ bobLosesGold ^? inventory . at "gold"
  print $ bobLosesGold ^. inventory . at "gold"
  print $ bobLosesGold ^? inventory . ix "gold"

  print $ bobLosesGold ^. score
  print $ bobLosesGold ^. userName

toListOfExamples :: IO ()
toListOfExamples = do
  let tory = HM.fromList [ ("gold", Item 99 10)
                         , ("silver", Item 10 9)
                         ]
      bob = User (UserName "bob") 42 Nothing tory

  print "A list of Bob's items"
  print $ bob ^.. inventory . folded
  print $ toListOf (inventory . folded) bob

  print "Bob uses ifodled . asIndex to list itemNames."
  print $ bob ^.. inventory . ifolded . asIndex

  print "Bob's filtering to only his valuable items."
  print $
    bob ^.. inventory . folded . filtered (\item -> (item ^. value) > 50)

hasGotcha :: IO ()
hasGotcha = do
  let bob = User (UserName "bob") 42 Nothing HM.empty

  print "Has bob gold in his inventory?"
  print $ has (inventory . at "gold") bob

hasGotchaIx :: IO ()
hasGotchaIx = do
  let bob = User (UserName "bob") 42 Nothing HM.empty
  print "Has bob gold in his inventory?"
  print $ has (inventory . ix "gold") bob

  let richBob = User (UserName "bob") 42 Nothing
                  $ HM.fromList [("gold", Item 10 10)]
  print "Has bob gold in his inventory?"
  print $ has (inventory . ix "gold") richBob
