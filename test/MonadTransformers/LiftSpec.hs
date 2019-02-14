-- http://taylor.fausak.me/2015/05/14/monad-transformers/
module MonadTransformers.LiftSpec where

import Test.Hspec
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, Reader, ask, runReader, runReaderT)
import Control.Monad.Trans.Writer (WriterT, Writer, tell, runWriter, runWriterT)

{-
  lift - takes a function, that works in one monad and allows
         us to use it in another.
         You can ask something from insider the Reader, but
         if you're in a transformer stack that contains a reader,
         you have to lift ask for it.

         Example:
         At the bottom is Identity. (Another popular one is IO ()).
         There is a ReaderT on top of that, it will hold a number.
         There is the WriterT on the top, which is like a 3-layered
         cake.
         We need to "lift" the `ask` over WriterT.

-}

type Output = Integer

anIdentity :: Identity Output
anIdentity = do
  x <- return 3
  let y = x * 2
  return y

type RInput = Integer
type ROutput = String

aReader :: Reader RInput ROutput
aReader = do
  x <- ask
  let s = "The input was " ++ show x
  pure s

anotherReader :: Reader RInput ROutput
anotherReader = do
  x <- ask
  let s = "I asked again for " ++ show x
  pure s

type WOutput = [String]
type WResult = Integer

aWriter :: Writer WOutput WResult
aWriter = do
  let x = 3
  tell ["The number was " ++ show x]
  return x

type SInput = Integer
type SOutput = [String]
type SResult = Integer

stack :: WriterT SOutput (ReaderT SInput Identity) SResult
stack = do
  x <- lift ask
  tell ["The input was " ++ show x]
  return x

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "Monad transformers" $ do
    describe "Identity monad" $
      it "almost does nothing, but useful" $
        runIdentity anIdentity `shouldBe` 6

    describe "Reader monad" $ do
      it "adds read-only data, used to read config" $
        runReader aReader 3 `shouldBe` "The input was 3"

      it "can run the reader through composed functions" $
        runReader (aReader >> anotherReader) 3
          `shouldBe` "I asked again for 3"

    describe "Writer monad" $
      it "adds write-only data, used for logging" $
        runWriter aWriter `shouldBe` (3,["The number was 3"])

    describe "Actual example with 3 layers of Monad stack" $
      it "needs lift to 'lift-over' WriterT" $ do
        let newReader = runWriterT stack
        let newIdentity = runReaderT newReader 3
        runIdentity newIdentity `shouldBe` (3,["The input was 3"])
