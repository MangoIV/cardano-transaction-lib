module Serialization.Hash
  ( Ed25519KeyHash
  , ScriptHash
  , ed25519KeyHashToBytes
  , ed25519KeyHashFromBytes
  , ed25519KeyHashFromBech32
  , ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  , scriptHashToBytes
  , scriptHashToBech32Unsafe
  , scriptHashFromBytes
  , scriptHashFromBech32
  , scriptHashToBech32
  , scriptHashAsBytes
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAesonViaJson
  , encodeAeson'
  )
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonString
  , encodeJson
  )
import Data.Argonaut as Json
import Data.Either (Either(Left), note)
import Data.Function (on)
import Data.Maybe (Maybe(Nothing))
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import FromData (class FromData)
import Metadata.FromMetadata (class FromMetadata)
import Metadata.ToMetadata (class ToMetadata, toMetadata)
import ToData (class ToData, toData)
import Types.Aliases (Bech32String)
import Types.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Types.PlutusData (PlutusData(Bytes))
import Types.TransactionMetadata (TransactionMetadatum(Bytes)) as Metadata

-- | PubKeyHash and StakeKeyHash refers to blake2b-224 hash digests of Ed25519
-- | verification keys
foreign import data Ed25519KeyHash :: Type

instance Eq Ed25519KeyHash where
  eq = eq `on` ed25519KeyHashToBytes

instance Ord Ed25519KeyHash where
  compare = compare `on` ed25519KeyHashToBytes

instance Show Ed25519KeyHash where
  show edkh = "(Ed25519KeyHash " <> byteArrayToHex (ed25519KeyHashToBytes edkh)
    <> ")"

instance ToData Ed25519KeyHash where
  toData = toData <<< ed25519KeyHashToBytes

instance FromData Ed25519KeyHash where
  fromData (Bytes kh) = ed25519KeyHashFromBytes kh
  fromData _ = Nothing

instance ToMetadata Ed25519KeyHash where
  toMetadata = toMetadata <<< ed25519KeyHashToBytes

instance FromMetadata Ed25519KeyHash where
  fromMetadata (Metadata.Bytes kh) = ed25519KeyHashFromBytes kh
  fromMetadata _ = Nothing

-- This is needed for `ApplyArgs`.
instance DecodeJson Ed25519KeyHash where
  -- ed25519KeyHashFromBech32 goes from Bech32String directly although this
  -- feels unsafe.
  decodeJson = caseJsonString
    (Left $ TypeMismatch "Expected Plutus BuiltinByteString")
    ( note (TypeMismatch "Invalid Ed25519KeyHash") <<< ed25519KeyHashFromBytes
        <=< note (TypeMismatch "Invalid ByteArray") <<< hexToByteArray
    )

instance DecodeAeson Ed25519KeyHash where
  decodeAeson = decodeAesonViaJson

instance EncodeAeson Ed25519KeyHash where
  encodeAeson' = encodeAeson' <<< byteArrayToHex <<< ed25519KeyHashToBytes

foreign import _ed25519KeyHashFromBytesImpl
  :: MaybeFfiHelper
  -> ByteArray
  -> Maybe Ed25519KeyHash

foreign import _ed25519KeyHashFromBech32Impl
  :: MaybeFfiHelper
  -> Bech32String
  -> Maybe Ed25519KeyHash

foreign import ed25519KeyHashToBytes :: Ed25519KeyHash -> ByteArray

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
foreign import ed25519KeyHashToBech32Unsafe
  :: String
  -> Ed25519KeyHash
  -> Bech32String

foreign import _ed25519KeyHashToBech32Impl
  :: MaybeFfiHelper
  -> String
  -> Ed25519KeyHash
  -> Maybe Bech32String

ed25519KeyHashFromBytes :: ByteArray -> Maybe Ed25519KeyHash
ed25519KeyHashFromBytes = _ed25519KeyHashFromBytesImpl maybeFfiHelper

ed25519KeyHashFromBech32 :: Bech32String -> Maybe Ed25519KeyHash
ed25519KeyHashFromBech32 = _ed25519KeyHashFromBech32Impl maybeFfiHelper

-- | Convert ed25519KeyHash to Bech32 representation with given prefix.
-- | Will return Nothing if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
ed25519KeyHashToBech32 :: String -> Ed25519KeyHash -> Maybe Bech32String
ed25519KeyHashToBech32 = _ed25519KeyHashToBech32Impl maybeFfiHelper

-- | blake2b-224 hash digests of serialized monetary scripts
foreign import data ScriptHash :: Type

instance Eq ScriptHash where
  eq = eq `on` scriptHashToBytes

instance Ord ScriptHash where
  compare = compare `on` scriptHashToBytes

instance Show ScriptHash where
  show edkh = "(ScriptHash " <> byteArrayToHex (scriptHashToBytes edkh) <> ")"

instance ToData ScriptHash where
  toData = toData <<< scriptHashToBytes

instance FromData ScriptHash where
  fromData (Bytes bytes) = scriptHashFromBytes bytes
  fromData _ = Nothing

instance ToMetadata ScriptHash where
  toMetadata = toMetadata <<< scriptHashToBytes

instance FromMetadata ScriptHash where
  fromMetadata (Metadata.Bytes bytes) = scriptHashFromBytes bytes
  fromMetadata _ = Nothing

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeJson ScriptHash where
  decodeJson =
    Json.caseJsonObject (Left (Json.TypeMismatch "Expected object")) $
      note (Json.TypeMismatch "Expected hex-encoded script hash")
        <<< (scriptHashFromBytes <=< hexToByteArray)
        <=< flip Json.getField "getScriptHash"

instance EncodeJson ScriptHash where
  encodeJson sh = encodeJson (scriptHashToBytes sh)

foreign import _scriptHashFromBytesImpl
  :: MaybeFfiHelper
  -> ByteArray
  -> Maybe ScriptHash

foreign import _scriptHashFromBech32Impl
  :: MaybeFfiHelper
  -> Bech32String
  -> Maybe ScriptHash

-- | Drops the type and returns the hash as a generic bytearray
foreign import scriptHashAsBytes :: ScriptHash -> ByteArray

-- | Encodes the hash to Cbor bytes
foreign import scriptHashToBytes :: ScriptHash -> ByteArray

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will crash if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
foreign import scriptHashToBech32Unsafe
  :: String
  -> ScriptHash
  -> Bech32String

foreign import _scriptHashToBech32Impl
  :: MaybeFfiHelper
  -> String
  -> ScriptHash
  -> Maybe Bech32String

-- | Decodes a script hash from its CBOR bytes encoding
-- | NOTE. It does _not_ compute hash of given bytes.
scriptHashFromBytes :: ByteArray -> Maybe ScriptHash
scriptHashFromBytes = _scriptHashFromBytesImpl maybeFfiHelper

-- | Decodes a script hash from its Bech32 representation
scriptHashFromBech32 :: Bech32String -> Maybe ScriptHash
scriptHashFromBech32 = _scriptHashFromBech32Impl maybeFfiHelper

-- | Convert scriptHash to Bech32 representation with given prefix.
-- | Will return `Nothing` if prefix is invalid (length, mixed-case, etc)
-- | More on prefixes: https://cips.cardano.org/cips/cip5
scriptHashToBech32 :: String -> ScriptHash -> Maybe Bech32String
scriptHashToBech32 = _scriptHashToBech32Impl maybeFfiHelper
