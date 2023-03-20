{-# LANGUAGE TypeOperators #-}


module Demo where

import           Data.Default
import           Plutarch (printTerm, Script)
import           Plutarch.Prelude
import           Plutarch.Api.V1.Contexts
import           Plutarch.Api.V1.Scripts
import           Eval
import           PAlwaysSucceedandFail
import           SignedBy 


fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
        $ self # (n - 1) + self # (n - 2)


main :: IO ()
main = do
  putStrLn $ printTerm def alwaysSucceeds
  putStrLn $ printTerm def alwaysFails
  putStrLn $ 

