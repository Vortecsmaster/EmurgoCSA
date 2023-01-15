{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module OnChain
    ( tokenPolicy
    , tokenCurSymbol
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Ledger                      hiding (mint, singleton)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Scripts
--import qualified Ledger.Typed.Scripts        as Scripts
import           Plutus.V1.Ledger.Value      as Value
import qualified Plutus.V1.Ledger.Interval   as Interval   

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TxOutRef -> TokenName -> Integer -> () -> ScriptContext -> Bool
mkTokenPolicy oref tn amt () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                                   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt')] -> tn' == tn && amt' == amt
        _                -> False

-- tokenPolicyScriptContext :: ScriptContext
-- tokenPolicyScriptContext = 
--     let st    = Value.assetClassValue adminAssetClass 1 -- Admin Token Class 1
--         datum = Datum (toBuiltinData $ SharesLockDatum mockSharesLockOwnerAddress stakePoolPubKeyHash shareClass1)
--     in  ScriptContext
--             { scriptContextTxInfo  = TxInfo
--             { txInfoInputs      = [ TxInInfo
--                                     (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0)
--                                     TxOut
--                                         { txOutAddress   = sharesLockScriptAddress mockAddFarmingRewardsTokenClass
--                                         , txOutValue     = Value.assetClassValue shareClass1 1_000
--                                                             <> singleton adaSymbol adaToken 2000000
--                                         , txOutDatumHash = Just (toDatumHash datum)
--                                         }
--                                     , TxInInfo
--                                     (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88bc" 1)
--                                     TxOut
--                                         { txOutAddress = signerAddress
--                                         , txOutValue = st <> singleton adaSymbol adaToken 5000000 <> Value.assetClassValue
--                                                         shareClass1
--                                                         1_000
--                                         , txOutDatumHash = Nothing
--                                         }
--                                     ]
--             , txInfoOutputs     = [ TxOut
--                                     { txOutAddress   = Address
--                                                         (ScriptCredential
--                                                             (sharesLockScriptValidatorHash mockAddFarmingRewardsTokenClass)
--                                                         )
--                                                         Nothing
--                                     , txOutValue     = Value.assetClassValue shareClass1 2_000
--                                                         <> singleton adaSymbol adaToken 5000000
--                                     , txOutDatumHash = Just (toDatumHash datum)
--                                     }
--                                     , TxOut { txOutAddress   = signerAddress
--                                             , txOutValue     = st <> singleton adaSymbol adaToken 2000000
--                                             , txOutDatumHash = Nothing
--                                             }
--                                     ]
--             , txInfoFee         = Value.singleton "" "" 2
--             , txInfoMint        = Value.singleton "Hello" (tokenCurSymbol (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0) "Hello" 5) 5
--             , txInfoDCert       = []
--             , txInfoWdrl        = []
--             , txInfoValidRange  = Interval.always
--             , txInfoSignatories = [signer]
--             , txInfoData        = [(toDatumHash datum, datum)]
--             , txInfoId          = "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88bd"
--             }
--             , scriptContextPurpose = Spending (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0)
--             }

-- tokenPolicyTest :: Bool
-- tokenPolicyTest = mkTokenPolicy (TxOutRef "0b2086cbf8b6900f8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0) "Hello" 5 () tokenPolicyScriptContext

-- tokenPolicyTestExpectFail :: Bool
-- tokenPolicyTestExpectFail = mkTokenPolicy (TxOutRef "0b2086cbf8b6900a8cb65e012de4516cb66b5cb08a9aaba12a8b88be" 0) "Hello" 5 () tokenPolicyScriptContext

-- succeeds :: Bool -> Bool
-- succeeds evalResult = evalResult

-- fails :: Bool -> Bool
-- fails = not evalResult

-- unitTests :: TestTree
-- unitTests = testGroup "mintingPolicyTests"
--     [
--         succeeds tokenPolicyTest,
--         fails tokenPolicyTestExpectFail
--     ]

tokenPolicy :: TxOutRef -> TokenName -> Integer -> Scripts.MintingPolicy
tokenPolicy oref tn amt = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' amt' -> Scripts.mkUntypedMintingPolicy $ mkTokenPolicy oref' tn' amt' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode amt


tokenCurSymbol :: TxOutRef -> TokenName -> Integer -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol . tokenPolicy oref tn

