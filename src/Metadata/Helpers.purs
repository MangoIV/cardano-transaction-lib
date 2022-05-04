module Metadata.Helpers
  ( mkKey
  , unsafeMkKey
  , lookupKey
  ) where

import Prelude

import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Foldable (lookup)
import Data.Newtype (wrap)
import Types.ByteArray (byteArrayFromAscii)
import Types.PlutusData (PlutusData(Map, Bytes))

mkKey :: String -> Maybe PlutusData
mkKey str = Bytes <<< wrap <$> byteArrayFromAscii str

unsafeMkKey :: Partial => String -> PlutusData
unsafeMkKey = fromJust <<< mkKey

lookupKey :: String -> PlutusData -> Maybe PlutusData
lookupKey keyStr (Map array) = mkKey keyStr >>= flip lookup array
lookupKey _ _ = Nothing
