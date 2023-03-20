{-# LANGUAGE RankNTypes #-}

module Eval (applyArguments, evalT, evalWithArgsT) where

import qualified Codec.CBOR.Write as Write
import Codec.Serialise (Serialise, encode)
import Data.Bifunctor (first)
import qualified Data.ByteString.Base16 as Base16
import Data.Default (def)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import Plutarch (ClosedTerm, compile)
import Plutarch.Evaluate (evalScript) 
import Plutarch.Script (Script (..))
import PlutusLedgerApi.V1 (Data, ExBudget)
--import PlutusLedgerApi.V1.Scripts (Script (unScript))
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import qualified PlutusCore                               as PLC
import qualified PlutusCore.Data                          as PLC
import qualified PlutusCore.DeBruijn                      as PLC
import qualified PlutusCore.Evaluation.Machine.ExBudget   as PLC
import qualified PlutusCore.MkPlc                         as PLC
import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC
import           Prelude                                  as Haskell

applyArguments :: Script -> [PLC.Data] -> Script
applyArguments (Script (UPLC.Program a v t)) args =
    let termArgs = Haskell.fmap (UPLC.termMapNames UPLC.unNameDeBruijn . PLC.mkConstant ()) args
        applied = PLC.mkIterApp () t termArgs
    in Script (UPLC.Program a v applied)

-- evalSerialize :: ClosedTerm a -> Haskell.Either Text Text
-- evalSerialize x = encodeSerialise . (\(a, _, _) -> a) <$> evalT x
--   where
--     encodeSerialise :: Serialise a => a -> Text
--     encodeSerialise = TE.decodeUtf8 . Base16.encode . Write.toStrictByteString . encode

evalT :: ClosedTerm a -> Haskell.Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Haskell.Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile def x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

-- evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
-- evalWithArgsT' x args =
--   (\(res, budg, trcs) -> (unScript res, budg, trcs))
--     <$> evalWithArgsT x args