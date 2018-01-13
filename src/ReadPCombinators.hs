{-# LANGUAGE OverloadedStrings #-}

module ReadPCombinators where

import Text.ParserCombinators.ReadP
import qualified Data.Text as T

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

readStringAs :: String -> a -> ReadP a
readStringAs s a = do
  _ <- string s
  return a

parseMaybe :: ReadP a -> T.Text -> Maybe a
parseMaybe parser input =
    case readP_to_S parser (T.unpack input) of
        [] -> Nothing
        ((result, _):_) -> Just result
