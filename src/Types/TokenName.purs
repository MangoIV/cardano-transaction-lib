module Types.TokenName
  ( TokenName
  , adaToken
  , getTokenName
  , mkTokenName
  , mkTokenNames
  , tokenNameFromAssetName
  , assetNameName
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.Bitraversable (ltraverse)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested (type (/\))
import FromData (class FromData)
import Serialization.Types (AssetName) as CSL
import ToData (class ToData)
import Types.RawBytes (RawBytes, byteLength)
import Data.Newtype (wrap, unwrap)

newtype TokenName = TokenName RawBytes

derive newtype instance Eq TokenName
derive newtype instance FromData TokenName
derive newtype instance Ord TokenName
derive newtype instance ToData TokenName

instance Show TokenName where
  show (TokenName tn) = "(TokenName" <> show tn <> ")"

getTokenName :: TokenName -> RawBytes
getTokenName (TokenName tokenName) = tokenName

-- | The empty token name.
adaToken :: TokenName
adaToken = TokenName mempty

-- | Create a `TokenName` from a `RawBytes` since TokenName data constructor is
-- | not exported
mkTokenName :: RawBytes -> Maybe TokenName
mkTokenName byteArr =
  if byteLength byteArr <= 32 then pure  <<< TokenName $ byteArr else Nothing

foreign import assetNameName :: CSL.AssetName -> RawBytes

tokenNameFromAssetName :: CSL.AssetName -> TokenName
tokenNameFromAssetName = TokenName <<< assetNameName

-- | Creates a Map of `TokenName` and Big Integers from a `Traversable` of 2-tuple
-- | `RawBytes` and Big Integers with the possibility of failure
mkTokenNames
  :: forall (t :: Type -> Type)
   . Traversable t
  => t (RawBytes /\ BigInt)
  -> Maybe (Map TokenName BigInt)
mkTokenNames = traverse (ltraverse mkTokenName) >>> map Map.fromFoldable
