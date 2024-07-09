module Main where

-- base
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Either (fromLeft)
import Text.Read (readEither)

-- text
import Data.Text as Text (Text, unpack)
import Data.Text qualified as Text (take)

-- koan
import Koan (WordCount (..))
import Koan qualified (main)

-- test-io
import TestIO

testLines :: [Text]
testLines =
  [ "Hello Rhine"
  , "this is a"
  , "test"
  ]

main :: IO ()
main = do
  putStrLn "No worries, this will take 10-20 seconds..."
  testForSecondsInput 10 (concat $ replicate 32500 testLines) Koan.main $ \output ->
    case output of
      [] -> ["Weird, your program didn't produce any output!"]
      _ -> fromLeft [] $ do
        (sinceInitText, countText) <- case take 3 output of
          [_, sinceInitText, countText] -> return (sinceInitText, countText)
          thing -> Left $ "Somehow there wasn't enough output:" : thing ++ ["Did you not include printCounts?"]
        (sinceInit :: Double) <- first (const ["Tried to parse sinceInit = ", sinceInitText, " but failed"]) $ readEither $ unpack sinceInitText
        (count :: WordCount) <- first (const ["Tried to parse count = ", countText, " but failed"]) $ readEither $ unpack countText
        let expected = WordCount {nChars = 877500, nWords = 195000, nLines = 97500}
        unless (Text.take 13 (last output) == "Final result:") $ Left ["Didn't find a 'Final result: ...' section."]
        unless (last output == "Final result: " <> tshow expected) $ Left ["Wrong count:", last output]
        unless
          ( nChars count <= nChars expected
              && nWords count <= nWords expected
              && nLines count <= nLines expected
          )
          $ Left ["The count seems too high:", tshow count]
        unless
          ( nChars count > 0
              && nWords count > 0
              && nLines count > 0
          )
          $ Left ["The count was 0:", tshow count]
        unless
          ( sinceInit > 0
              && sinceInit < 2
          )
          $ Left ["There was no count message after one second, but after: " <> tshow sinceInit]
