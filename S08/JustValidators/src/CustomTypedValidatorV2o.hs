{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}


module CustomTypedValidatorV2o where

--PlutusTx 
import           PlutusTx                             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as Builtins
import           PlutusTx.Prelude                     hiding (Semigroup(..), unless)
--Contract Monad
import           Plutus.Contract               
--Ledger 
import           Ledger                               hiding (singleton)
import qualified Ledger.Address                       as V1Address
import           Ledger.Constraints                   as Constraints              -- Same library name, different functions for V1 and V2 in some cases
--import qualified Ledger.Typed.Scripts               as Scripts
import           Ledger.Typed.Scripts                 as UScripts             
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts            -- New library name for Typed Validators and some new fuctions
import qualified Plutus.V2.Ledger.Api                 as PlutusV2            
import           Ledger.Ada                           as Ada

{-
import Language.Haskell.TH (RuleBndr(TypedRuleVar))
-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--THE ON-CHAIN CODE

-- newtype MyWonderfullRedeemer = MWR Integer

-- PlutusTx.makeIsDataIndexed ''MyWonderfullRedeemer [('MWR,0)]        -- At compile time write an instance of this data type (MyWonderFullRedeemer) on the IsData typeclass

data MyWonderfullRedeemer = MWR Integer
PlutusTx.unstableMakeIsData ''MyWonderfullRedeemer


{-# INLINABLE typedRedeemer #-} 
typedRedeemer :: () -> MyWonderfullRedeemer -> PlutusV2.ScriptContext -> Bool   
typedRedeemer () (MWR redeemer) _ = traceIfFalse "Wrong redeemer!" (redeemer == 42)
 

data Typed                                                          -- New type that encode the information about the Datum and the Redeemer
instance Scripts.ValidatorTypes Typed where
     type instance RedeemerType Typed = MyWonderfullRedeemer
     type instance DatumType Typed = ()

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed      
    $$(PlutusTx.compile [|| typedRedeemer ||]) 
    $$(PlutusTx.compile [|| wrap ||])                               
  where
    wrap = UScripts.mkUntypedValidator   --New wrapper function for typed validators             
    

validator :: Validator
validator = Scripts.validatorScript typedValidator                   -- Get the untyped validator script of the wrapped typeValidator PlutusCore

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator                      

scrAddress :: Ledger.Address
scrAddress = Scripts.validatorAddress typedValidator                 -- New functino to derive the address, included in the Utils library
--scrAddress = scriptAddress validator 