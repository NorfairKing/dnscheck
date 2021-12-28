{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNSCheckSpec (spec) where

import DNSCheck
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text
import Data.IP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.DNS
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

instance GenValid IPv4 where
  genValid = toIPv4w <$> genValid
  shrinkValid = fmap toIPv4w . shrinkValid . fromIPv4w

instance GenValid IPv6 where
  genValid = toIPv6w <$> genValid
  shrinkValid = fmap toIPv6w . shrinkValid . fromIPv6w

instance GenValid MXValue where
  genValid = MXValue <$> genValid <*> genValidDomain

instance GenValid ACheck where
  genValid =
    ACheck
      <$> genValidDomain
      <*> genValid

instance GenValid AAAACheck where
  genValid =
    AAAACheck
      <$> genValidDomain
      <*> genValid

instance GenValid MXCheck where
  genValid =
    MXCheck
      <$> genValidDomain
      <*> genValid

instance GenValid TXTCheck where
  genValid =
    TXTCheck
      <$> genValidDomain
      <*> genValid

instance GenValid NSCheck where
  genValid =
    NSCheck
      <$> genValidDomain
      <*> genListOf genValidDomain

instance GenValid CNAMECheck where
  genValid =
    CNAMECheck
      <$> genValidDomain
      <*> genListOf genValidDomain

instance GenValid Check

instance GenValid RetryPolicySpec

instance GenValid CheckSpec

genValidDomain :: Gen Domain
genValidDomain =
  TE.encodeUtf8
    <$> genTextBy
      ( oneof
          [ choose ('a', 'z'),
            pure '.'
          ]
      )
      `suchThat` (not . T.null)

spec :: Spec
spec = do
  jsonSpec @IPv4
  genValidSpec @IPv4
  jsonSpec @IPv6
  genValidSpec @IPv6
  jsonSpec @MXValue
  genValidSpec @MXValue
  describe "parseMXValue" $
    it "roundtrips with renderMXValue" $
      forAllValid $ \mxValue ->
        parseMXValue (renderMXValue mxValue) `shouldBe` Right mxValue
  jsonSpec @ACheck
  genValidSpec @ACheck
  jsonSpec @AAAACheck
  genValidSpec @AAAACheck
  jsonSpec @MXCheck
  genValidSpec @MXCheck
  jsonSpec @TXTCheck
  genValidSpec @TXTCheck
  jsonSpec @CNAMECheck
  genValidSpec @CNAMECheck
  jsonSpec @NSCheck
  genValidSpec @NSCheck
  jsonSpec @Check
  genValidSpec @Check
  jsonSpec @RetryPolicySpec
  genValidSpec @RetryPolicySpec
  jsonSpec @CheckSpec
  genValidSpec @CheckSpec
