module NaiveSpeelChecker where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

editDistance :: Text -> Text -> Int
editDistance stringA stringB
  | T.null stringA = T.length stringB
  | T.null stringB = T.length stringA
  | T.head stringA == T.head stringB = editDistance restOfA restOfB
  | otherwise = 1 + minimum [insertCost, deleteCost, swapCost]
  where
    restOfA = T.tail stringA
    restOfB = T.tail stringB
    deleteCost = editDistance restOfA stringB
    insertCost = editDistance stringA restOfB
    swapCost = editDistance restOfA restOfB

data SuggestedMatch = SuggestedMatch
  { matchWord :: Text,
    matchSearchedWord :: Text,
    matchDistance :: Int
  }
  deriving (Eq, Show)

showSuggestedMatch :: SuggestedMatch -> String
showSuggestedMatch s =
  printf "%s -> %s: %d" s.matchWord s.matchSearchedWord s.matchDistance

spellCheckWord :: [Text] -> Int -> Text -> [SuggestedMatch]
spellCheckWord dictionary threshold word =
  getSuggestions dictionary []
  where
    getSuggestions [] suggestions = suggestions
    getSuggestions (dictWord:dict) suggestions
        | distance == 0 = []
        | distance > threshold = getSuggestions dict suggestions
        | otherwise = getSuggestions dict (suggestion : suggestions)
        where
          distance = editDistance dictWord word
          suggestion = SuggestedMatch dictWord word distance
