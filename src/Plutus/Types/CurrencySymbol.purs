module Plutus.Types.CurrencySymbol
  ( CurrencySymbol
  , adaSymbol
  , currencyMPSHash
  , getCurrencySymbol
  , mkCurrencySymbol
  , mpsSymbol
  , scriptHashAsCurrencySymbol
  ) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , decodeJson
  , encodeJson
  , getField
  )
import Data.Either (Either(Left))
import Data.Maybe (Maybe)
import FromData (class FromData)
import Serialization.Hash
  ( ScriptHash
  , scriptHashAsBytes
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import ToData (class ToData)
import Types.ByteArray (ByteArray)
import Types.Scripts (MintingPolicyHash(MintingPolicyHash))

newtype CurrencySymbol = CurrencySymbol ByteArray

derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance ToData CurrencySymbol

instance DecodeJson CurrencySymbol where
  decodeJson = caseJsonObject
    (Left $ TypeMismatch "Expected object")
    (flip getField "unCurrencySymbol" >=> decodeJson >>> map CurrencySymbol)

instance EncodeJson CurrencySymbol where
  encodeJson (CurrencySymbol mph) = encodeJson
    { "unCurrencySymbol": encodeJson mph }

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol" <> show cs <> ")"

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

scriptHashAsCurrencySymbol :: ScriptHash -> CurrencySymbol
scriptHashAsCurrencySymbol = CurrencySymbol <<< scriptHashAsBytes

-- | The minting policy hash of a currency symbol.
currencyMPSHash :: CurrencySymbol -> Maybe MintingPolicyHash
currencyMPSHash = map MintingPolicyHash <<< currencyScriptHash

-- | The currency symbol of a monetary policy hash.
mpsSymbol :: MintingPolicyHash -> Maybe CurrencySymbol
mpsSymbol (MintingPolicyHash h) = mkCurrencySymbol $ scriptHashToBytes h

getCurrencySymbol :: CurrencySymbol -> ByteArray
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

mkCurrencySymbol :: ByteArray -> Maybe CurrencySymbol
mkCurrencySymbol byteArr
  | byteArr == mempty =
      pure adaSymbol
  | otherwise =
      scriptHashFromBytes byteArr $> CurrencySymbol byteArr

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

currencyScriptHash :: CurrencySymbol -> Maybe ScriptHash
currencyScriptHash = scriptHashFromBytes <<< getCurrencySymbol
