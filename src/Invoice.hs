{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE QualifiedDo       #-}
{-# LANGUAGE DeriveAnyClass    #-}


module Invoice where

import           Prelude.Linear

import qualified Control.Functor.Linear   as Linear
import           Data.Unrestricted.Linear
import qualified System.IO.Linear         as LIO
import qualified System.IO.Resource       as Linear
import qualified Unsafe.Linear            as Unsafe




regularIdentity :: a -> a
regularIdentity x = x


linearIdentity :: a %1-> a
linearIdentity x = x




nonLinearDup :: Ur a %1-> (a, a)
nonLinearDup (Ur x) = (x, x)






linearAdd :: Int %1-> Int %1-> Int
linearAdd = (+)

const' :: a %1-> b -> a
const' a _ = a

-- dup1 :: a %1-> (a, Int)
-- dup1 x = (getX 10, 10)
--   where getX :: forall b. b -> a
--         getX  = const' x
-- 




linearAdd2 :: Int %1-> Int
linearAdd2 = linearAdd 2














type LIO = LIO.IO

data InvoiceState = Draft | Issued | Paid deriving (Show, Eq)

data Invoice (s :: InvoiceState) where
  InvoiceDraft  :: Ur String %1-> Ur Int %1-> Invoice Draft
  InvoiceIssued :: Ur String %1-> Ur Int %1-> Invoice Issued
  InvoicePaid   :: Ur String %1-> Ur Int %1-> Invoice Paid




mkInvoice :: String -> Int -> LIO (Invoice Draft)
mkInvoice s i = Linear.return $ InvoiceDraft (Ur s) (Ur i)


saveInvoiceToDB :: Invoice Draft %1-> LIO (Invoice Issued)
saveInvoiceToDB (InvoiceDraft s i) = Linear.return $ InvoiceIssued s i


issueInvoice :: Invoice Draft %1-> LIO (Invoice Issued)
issueInvoice draft = saveInvoiceToDB draft


process :: a %1-> LIO ()
process = Unsafe.coerce


savePaymentToDB :: String %1-> Int %1-> LIO ()
savePaymentToDB s i = Linear.do
  process s
  process i
  Linear.return ()


payForInvoice :: Invoice Issued %1-> LIO (Invoice Paid)
payForInvoice (InvoiceIssued (Ur s) (Ur i)) = Linear.do
  savePaymentToDB s i
  Linear.return $ InvoicePaid (Ur s) (Ur i)


runInvoicing :: LIO (Invoice Paid)
runInvoicing = Linear.do
  draft  <- mkInvoice "restaumatic" 123
  issued <- issueInvoice draft
  payForInvoice issued


