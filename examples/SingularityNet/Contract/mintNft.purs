module SingularityNet.Contract.MintNft
  ( mintBondedStateNft
  ) where

import Contract.Prelude
import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad
  ( Contract
  , liftContractE'
  , liftContractM
  , liftedE'
  , liftedM
  )
import Contract.Prim.ByteArray
  ( byteArrayFromIntArray
  , byteArrayFromString
  )
import Contract.PlutusData (PlutusData, Datum(Datum), toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , TransactionInput(TransactionInput)
  , TransactionHash(TransactionHash)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustPayToOtherScript
  , mustMintValue
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkSingletonValue', mkTokenName, scriptCurrencySymbol)
import SingularityNet.MintingPolicy (nftMintingPolicy)
import SingularityNet.Types (BondedStakingState(BondedStakingState))
import SingularityNet.Validator (bondedValidator)

mintBondedStateNft :: Contract () Unit
mintBondedStateNft = do
  networkId <- getNetworkId
  pkh <- liftedM "mintBondedStateNft: Cannot get pkh" ownPaymentPubKeyHash
  log $ "My pkh: " <> show pkh
  -- Get the (Nami) wallet address
  userAddr <- liftedM "mintBondedStateNft: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "mintBondedStateNft: Cannot get user Utxos" (utxosAt userAddr)
  log $ "My utxos: " <> show userUtxos
  transactionId <- liftContractM "mintBondedStateNft: invalid TxHash" $ TransactionHash <$>
    byteArrayFromIntArray
      [244,221,176,254,7,61,205,6,242,82,183,90,10,71,248,211,186,77,144,189,150,5,121,227,170,63,237,34,26,183,133,84]
  let
    utxo = TransactionInput
      { transactionId
      , index: zero
      }
  -- Get the minting policy, the currency symbol and token name:
  policy <- liftedE' $ pure nftMintingPolicy
  log $ show $ policy
  curr <- liftedM "mintBondedStateNft: Cannot get CurrencySymbol"
    (scriptCurrencySymbol policy)
  -- May want to hardcode this somewhere:
  tokenName <- liftContractM "mintBondedStateNft: Cannot create TokenName"
    $ mkTokenName
    =<< byteArrayFromString "BondedStakingToken"
  mintValue <- liftContractM "mintBondedStateNft: Cannot create NFT Value"
    (mkSingletonValue' curr tokenName one)
  -- Get the bonding validator and hash
  validator <- liftContractE' bondedValidator
  valHash <- liftedM "mintBondedStateNft: Cannot hash validator"
    (validatorHash validator)
  let scriptAddr = validatorHashEnterpriseAddress networkId valHash
  log $ "validatorAddress: " <> show scriptAddr
  let
    bondedStateDatum = Datum $ toData $ BondedStakingState []

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.otherScript validator
      , ScriptLookups.unspentOutputs (unwrap userUtxos)
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToOtherScript valHash bondedStateDatum mintValue
        , mustMintValue mintValue
        , mustSpendPubKeyOutput utxo
        ]

  unattachedBalancedTx <-
    liftedE' (ScriptLookups.mkUnbalancedTx lookup constraints)
  -- `balanceAndSignTx` does the following:
  -- 1) Balance a transaction
  -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
  -- 3) Attach datums and redeemers to transaction.
  -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
  BalancedSignedTransaction { signedTxCbor } <- liftedM
    "mintBondedStateNft: Cannot balance, reindex redeemers, attach datums/\
    \redeemers and sign"
    (balanceAndSignTx unattachedBalancedTx)
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  log $ "mintBondedStateNft: Transaction successfully submitted with hash: "
    <> show transactionHash