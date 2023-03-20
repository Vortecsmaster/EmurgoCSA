{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}


module SignedBy where

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Api.V1.Crypto
import Plutarch.Api.V1.Scripts
import qualified Plutarch.Monadic as P
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval
import qualified PlutusTx

checkSignatory :: Term s (PPubKeyHash :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
checkSignatory = plam $ \ph _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif
    (pelem # pdata ph # pfromData signatories)
    -- Success!
    (pconstant ())
    -- Signature not present.
    perror

-- EVALUATE PLUTARCH SCRIPT

hashStr :: PubKeyHash
hashStr = "abce0f123e"

pubKeyHash :: Term s PPubKeyHash
pubKeyHash = pconstant hashStr

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
      [fromString hashStr, "f013", "ab45"]
      mempty
      ""
    )
    (Spending (TxOutRef "" 1))

signedEval = evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]si
--Right (Program () (Version () 1 0 0) (Constant () (Some (ValueOf unit ()))))