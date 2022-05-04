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

import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap)
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Serialization.Hash
  ( ScriptHash
  , scriptHashAsBytes
  , scriptHashFromBytes
  , scriptHashToBytes
  )
import Types.ByteArray (ByteArray)
import Types.CborBytes (CborBytes, cborBytesToRawBytes)
import Types.RawBytes (RawBytes)
import Types.Scripts (MintingPolicyHash(MintingPolicyHash))

newtype CurrencySymbol = CurrencySymbol RawBytes

derive newtype instance Eq CurrencySymbol
derive newtype instance Ord CurrencySymbol
derive newtype instance FromData CurrencySymbol
derive newtype instance ToData CurrencySymbol

instance Show CurrencySymbol where
  show (CurrencySymbol cs) = "(CurrencySymbol" <> show cs <> ")"

adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol mempty

-- FIXME: now what is it exactly?
scriptHashAsCurrencySymbol :: ScriptHash -> CurrencySymbol
scriptHashAsCurrencySymbol = CurrencySymbol <<< wrap <<< unwrap <<< scriptHashAsBytes

-- | The minting policy hash of a currency symbol.
currencyMPSHash :: CurrencySymbol -> Maybe MintingPolicyHash
currencyMPSHash = map MintingPolicyHash <<< currencyScriptHash

-- | The currency symbol of a monetary policy hash.
mpsSymbol :: MintingPolicyHash -> Maybe CurrencySymbol
mpsSymbol (MintingPolicyHash h) = mkCurrencySymbol <<< cborBytesToRawBytes $ scriptHashToBytes h

getCurrencySymbol :: CurrencySymbol -> RawBytes
getCurrencySymbol (CurrencySymbol curSymbol) = curSymbol

mkCurrencySymbol :: RawBytes -> Maybe CurrencySymbol
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
