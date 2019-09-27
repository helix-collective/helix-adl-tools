{-# LANGUAGE OverloadedStrings #-}
module Utils where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Char(isUpper)

toSnakeCase :: T.Text -> T.Text
toSnakeCase = useLazy (LT.intercalate "_" . words)
  where
  useLazy :: (LT.Text -> LT.Text) -> T.Text -> T.Text
  useLazy f = LT.toStrict .  f . LT.fromStrict

  words :: LT.Text -> [LT.Text]
  words t | LT.null t = []
          | otherwise =
    let (w,t1) = word t
    in w:words t1

  word :: LT.Text -> (LT.Text,LT.Text)
  word t =
    let (uc,t1) = LT.span isUpper t
        (nuc,t2) = LT.span (not . isUpper) t1
    in (LT.toLower (uc <> nuc), t2)

