{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNSCheck where

import Autodocodec
import Autodocodec.Yaml
import Control.Retry
import Data.ByteString (ByteString)
import Data.IP
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Validity
import Data.Validity.ByteString ()
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.DNS
import Network.DNS.Lookup as DNS
import Network.DNS.LookupRaw as DNS
import Network.DNS.Utils as DNS
import Path
import Path.IO
import System.Environment
import System.Exit
import Test.Syd
import Test.Syd.OptParse (defaultSettings)
import Text.Read

dnsCheck :: IO ()
dnsCheck = do
  args <- getArgs
  case args of
    [] -> die "Supply a spec file path as an argument"
    (sfp : _) -> do
      afp <- resolveFile' sfp
      mspec <- readYamlConfigFile afp
      case mspec of
        Nothing -> die $ "Spec file not found: " <> fromAbsFile afp
        Just spec -> runCheckSpec spec

runCheckSpec :: CheckSpec -> IO ()
runCheckSpec cs =
  sydTestWith defaultSettings (checkSpec cs)

checkSpec :: CheckSpec -> Spec
checkSpec CheckSpec {..} =
  aroundAll
    ( \func -> do
        rs <- makeResolvSeed defaultResolvConf
        withResolver rs $ \resolver ->
          func resolver
    )
    $ doNotRandomiseExecutionOrder
    $ mapM_ (singleCheckSpec specRetryPolicy) specChecks

singleCheckSpec ::
  RetryPolicySpec ->
  Check ->
  TestDef '[Resolver] ()
singleCheckSpec retryPolicySpec =
  let dnsError err = expectationFailure (show err)
      retryDNS = retryDNSWithPolicy retryPolicySpec
   in \case
        CheckA (ACheck domain expectedIpv4s) -> domainIt domain "A" $ \resolver -> do
          errOrIps <- retryDNS $ DNS.lookupA resolver domain
          case errOrIps of
            Left err -> dnsError err
            Right actualIpv4s -> actualIpv4s `shouldBe` expectedIpv4s
        CheckIPv6 (AAAACheck domain expectedIpv6s) -> domainIt domain "AAAA" $ \resolver -> do
          errOrIps <- retryDNS $ DNS.lookupAAAA resolver domain
          case errOrIps of
            Left err -> dnsError err
            Right actualIpv6s -> actualIpv6s `shouldBe` expectedIpv6s
        CheckMX (MXCheck domain expectedDomains) -> domainIt domain "MX" $ \resolver -> do
          errOrDomains <- retryDNS $ DNS.lookupMX resolver domain
          case errOrDomains of
            Left err -> dnsError err
            Right actualDomains ->
              sort (map (uncurry $ flip MXValue) actualDomains) `shouldBe` sort expectedDomains
        CheckTXT (TXTCheck domain expectedValues) -> domainIt domain "TXT" $ \resolver -> do
          errOrValues <- retryDNS $ DNS.lookupTXT resolver domain
          case errOrValues of
            Left err -> dnsError err
            Right actualValues ->
              actualValues `shouldBe` map TE.encodeUtf8 expectedValues
        CheckCNAME (CNAMECheck domain expectedValues) -> domainIt domain "CNAME" $ \resolver -> do
          errOrDNSMessage <- retryDNS $ DNS.lookupRaw resolver domain CNAME
          case errOrDNSMessage >>= (`fromDNSMessage` parseCNAMEDNSMessage) of
            Left err -> dnsError err
            Right actualValues ->
              actualValues `shouldBe` expectedValues
        CheckNS (NSCheck domain expectedValues) -> domainIt domain "NS" $ \resolver -> do
          errOrValues <- retryDNS $ DNS.lookupNS resolver domain
          case errOrValues of
            Left err -> dnsError err
            Right actualValues -> sort actualValues `shouldBe` sort expectedValues

domainIt :: Domain -> String -> (Resolver -> IO ()) -> TestDef '[Resolver] ()
domainIt d s = itWithOuter (unwords [s, show d])

retryDNSWithPolicy ::
  RetryPolicySpec ->
  IO (Either DNSError a) ->
  IO (Either DNSError a)
retryDNSWithPolicy retryPolicySpec action =
  retrying
    (retryPolicySpecToRetryPolicy retryPolicySpec)
    (\_ e -> pure (couldBeFlaky e))
    (const action)
  where
    couldBeFlaky (Left e) = case e of
      RetryLimitExceeded -> True
      TimeoutExpired -> True
      ServerFailure -> True
      NameError -> True
      NetworkFailure _ -> True
      DecodeError _ -> True
      UnknownDNSError -> True
      _ -> False
    couldBeFlaky _ = False

parseCNAMEDNSMessage :: DNSMessage -> [Domain]
parseCNAMEDNSMessage = mapMaybe go . answer
  where
    go :: ResourceRecord -> Maybe Domain
    go ResourceRecord {..} =
      if rrtype /= CNAME
        then Nothing
        else case rdata of
          RD_CNAME d -> Just d
          _ -> Nothing

data CheckSpec = CheckSpec
  { specRetryPolicy :: !RetryPolicySpec,
    specChecks :: ![Check]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CheckSpec)

instance Validity CheckSpec

instance HasCodec CheckSpec where
  codec =
    object "CheckSpec" $
      CheckSpec
        <$> optionalFieldWithDefault "retry-policy" defaultRetryPolicySpec "The retry policy for flaky checks due to network failures etc" .= specRetryPolicy
        <*> requiredField "checks" "The checks to perform" .= specChecks

data RetryPolicySpec = RetryPolicySpec
  { retryPolicySpecMaxRetries :: !Word,
    retryPolicySpecBaseDelay :: !Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RetryPolicySpec)

instance Validity RetryPolicySpec

instance HasCodec RetryPolicySpec where
  codec =
    object "RetryPolicySpec" $
      RetryPolicySpec
        <$> optionalFieldWithDefault "max-retries" (retryPolicySpecMaxRetries defaultRetryPolicySpec) "The maximum number of retries" .= retryPolicySpecMaxRetries
        <*> optionalFieldWithDefault "base-delay" (retryPolicySpecBaseDelay defaultRetryPolicySpec) "The delay between the first and second try, in microseconds" .= retryPolicySpecBaseDelay

defaultRetryPolicySpec :: RetryPolicySpec
defaultRetryPolicySpec =
  RetryPolicySpec
    { retryPolicySpecMaxRetries = 10,
      retryPolicySpecBaseDelay = 100_000 -- 100 ms
    }

retryPolicySpecToRetryPolicy :: RetryPolicySpec -> RetryPolicyM IO
retryPolicySpecToRetryPolicy RetryPolicySpec {..} =
  mconcat
    [ exponentialBackoff $ fromIntegral retryPolicySpecBaseDelay,
      limitRetries $ fromIntegral retryPolicySpecMaxRetries
    ]

data Check
  = CheckA !ACheck
  | CheckIPv6 !AAAACheck
  | CheckMX !MXCheck
  | CheckTXT !TXTCheck
  | CheckCNAME !CNAMECheck
  | CheckNS !NSCheck
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Check)

instance Validity Check

instance HasCodec Check where
  codec =
    dimapCodec f g $ eitherCodec (eitherCodec (eitherCodec codec codec) codec) (eitherCodec (eitherCodec codec codec) codec)
    where
      f = \case
        Left (Left (Left aCheck)) -> CheckA aCheck
        Left (Left (Right aaaaCheck)) -> CheckIPv6 aaaaCheck
        Left (Right mxCheck) -> CheckMX mxCheck
        Right (Left (Left txtCheck)) -> CheckTXT txtCheck
        Right (Left (Right cnameCheck)) -> CheckCNAME cnameCheck
        Right (Right nsCheck) -> CheckNS nsCheck
      g = \case
        CheckA aCheck -> Left (Left (Left aCheck))
        CheckIPv6 aaaaCheck -> Left (Left (Right aaaaCheck))
        CheckMX mxCheck -> Left (Right mxCheck)
        CheckTXT txtCheck -> Right (Left (Left txtCheck))
        CheckCNAME cnameCheck -> Right (Left (Right cnameCheck))
        CheckNS nsCheck -> Right (Right nsCheck)

validateDomain :: Domain -> Validation
validateDomain domain =
  declare "The domain is normalised" $
    parseDomain (renderDomain domain) == domain

data ACheck = ACheck
  { aCheckDomain :: !Domain,
    aCheckAddresses :: ![IPv4]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ACheck)

instance Validity ACheck where
  validate aCheck@ACheck {..} =
    mconcat
      [ genericValidate aCheck,
        validateDomain aCheckDomain
      ]

instance HasCodec ACheck where
  codec =
    object "ACheck" $
      typeField "a" ACheck
        <*> domainField .= aCheckDomain
        <*> singleOrListField "ip" "ips" "a addresses" .= aCheckAddresses

instance Validity IPv4

deriving via (Autodocodec IPv4) instance (FromJSON IPv4)

deriving via (Autodocodec IPv4) instance (ToJSON IPv4)

instance HasCodec IPv4 where
  codec =
    bimapCodec
      ( \s -> case readMaybe s of
          Nothing -> Left "Un-Read-able IPv4 address"
          Just a -> Right a
      )
      show
      codec

data AAAACheck = AAAACheck
  { aaaaCheckDomain :: !Domain,
    aaaaCheckAddresses :: ![IPv6]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AAAACheck)

instance Validity AAAACheck where
  validate aaaaCheck@AAAACheck {..} =
    mconcat
      [ genericValidate aaaaCheck,
        validateDomain aaaaCheckDomain
      ]

instance HasCodec AAAACheck where
  codec =
    object "AAAACheck" $
      typeField "aaaa" AAAACheck
        <*> domainField .= aaaaCheckDomain
        <*> singleOrListField "ip" "ips" "ipv6 addresses" .= aaaaCheckAddresses

instance Validity IPv6

deriving via (Autodocodec IPv6) instance (FromJSON IPv6)

deriving via (Autodocodec IPv6) instance (ToJSON IPv6)

instance HasCodec IPv6 where
  codec =
    bimapCodec
      ( \s -> case readMaybe s of
          Nothing -> Left "Un-Read-able IPv6 address"
          Just ipv6 -> Right ipv6
      )
      show
      codec

data MXCheck = MXCheck
  { mxCheckDomain :: !Domain,
    mxCheckAddresses :: ![MXValue]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec MXCheck)

instance Validity MXCheck

instance HasCodec MXCheck where
  codec =
    object "MXCheck" $
      typeField "mx" MXCheck
        <*> domainField .= mxCheckDomain
        <*> singleOrListField "value" "values" "values: domain and priority" .= mxCheckAddresses

data MXValue = MXValue
  { mxValuePriority :: !Int,
    mxValueDomain :: !Domain
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec MXValue)

instance Validity MXValue where
  validate mxValue@MXValue {..} =
    mconcat
      [ genericValidate mxValue,
        validateDomain mxValueDomain
      ]

instance HasCodec MXValue where
  codec = bimapCodec parseMXValue renderMXValue codec

parseMXValue :: Text -> Either String MXValue
parseMXValue t = case T.words t of
  [pt, dt] -> do
    let domain = parseDomain dt
    case readMaybe (T.unpack pt) of
      Nothing -> Left $ unwords ["Failed to parse MX record priority:", show pt]
      Just priority -> pure $ MXValue priority domain
  _ -> Left "Could not parse MX value"

renderMXValue :: MXValue -> Text
renderMXValue MXValue {..} =
  T.unwords
    [ T.pack $ show mxValuePriority,
      renderDomain mxValueDomain
    ]

data TXTCheck = TXTCheck
  { txtCheckDomain :: !Domain,
    txtCheckAddresses :: ![Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TXTCheck)

instance Validity TXTCheck where
  validate txtCheck@TXTCheck {..} =
    mconcat
      [ genericValidate txtCheck,
        validateDomain txtCheckDomain
      ]

instance HasCodec TXTCheck where
  codec =
    object "TXTCheck" $
      typeField "txt" TXTCheck
        <*> domainField .= txtCheckDomain
        <*> singleOrListField "value" "values" "text values" .= txtCheckAddresses

data CNAMECheck = CNAMECheck
  { cnameCheckDomain :: !Domain,
    cnameCheckAddresses :: ![Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CNAMECheck)

instance Validity CNAMECheck where
  validate cnameCheck@CNAMECheck {..} =
    mconcat
      [ genericValidate cnameCheck,
        validateDomain cnameCheckDomain,
        decorateList cnameCheckAddresses validateDomain
      ]

instance HasCodec CNAMECheck where
  codec =
    object "CNAMECheck" $
      typeField "cname" CNAMECheck
        <*> domainField .= cnameCheckDomain
        <*> singleOrListFieldWith "value" "values" domainCodec "domains" .= cnameCheckAddresses

data NSCheck = NSCheck
  { nsCheckDomain :: !Domain,
    nsCheckAddresses :: ![Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec NSCheck)

instance Validity NSCheck where
  validate nsCheck@NSCheck {..} =
    mconcat
      [ genericValidate nsCheck,
        validateDomain nsCheckDomain,
        decorateList nsCheckAddresses validateDomain
      ]

instance HasCodec NSCheck where
  codec =
    object "NSCheck" $
      typeField "ns" NSCheck
        <*> domainField .= nsCheckDomain
        <*> singleOrListFieldWith "value" "values" domainCodec "domains" .= nsCheckAddresses

typeField :: Text -> a -> ObjectCodec b a
typeField typeName a =
  a <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

domainField :: JSONObjectCodec Domain
domainField =
  requiredFieldWith
    "domain"
    (dimapCodec parseDomain (TE.decodeUtf8With TE.lenientDecode) codec)
    "domain"

singleOrListField :: HasCodec a => Text -> Text -> Text -> JSONObjectCodec [a]
singleOrListField singleKey listKey = singleOrListFieldWith singleKey listKey codec

singleOrListFieldWith :: Text -> Text -> JSONCodec a -> Text -> JSONObjectCodec [a]
singleOrListFieldWith singleKey listKey c doc =
  dimapCodec f g $
    eitherCodec
      (requiredFieldWith singleKey c doc)
      (requiredFieldWith listKey (listCodec c) doc)
  where
    f = \case
      Left v -> [v]
      Right vs -> vs
    g = \case
      [v] -> Left v
      vs -> Right vs

domainCodec :: JSONCodec Domain
domainCodec = dimapCodec parseDomain renderDomain codec

parseDomain :: Text -> Domain
parseDomain = DNS.normalize . TE.encodeUtf8

renderDomain :: Domain -> Text
renderDomain = TE.decodeUtf8With TE.lenientDecode

bytestringTextValueCodec :: JSONCodec ByteString
bytestringTextValueCodec = dimapCodec TE.encodeUtf8 (TE.decodeUtf8With TE.lenientDecode) codec
