{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNSCheck
  ( dnsCheck,
  )
where

import Control.Retry
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IP
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (FromJSON (..))
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
import YamlParse.Applicative

dnsCheck :: IO ()
dnsCheck = do
  args <- getArgs
  case args of
    [] -> die "Supply a spec file path as an argument"
    (sfp : _) -> do
      afp <- resolveFile' sfp
      mspec <- readConfigFile afp
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
    $ mapM_ (singleCheckSpec specRetryPolicy) specChecks

singleCheckSpec ::
  RetryPolicySpec ->
  Check ->
  TestDef '[Resolver] ()
singleCheckSpec retryPolicySpec =
  let domainIt :: Domain -> String -> (Resolver -> IO ()) -> TestDef '[Resolver] ()
      domainIt d s = itWithOuter (unwords [s, show d])
      dnsError err = expectationFailure (show err)
      retryDNS = retryDNSWithPolicy retryPolicySpec
   in \case
        CheckA domain expectedIpv4s -> domainIt domain "A" $ \resolver -> do
          errOrIps <- retryDNS $ DNS.lookupA resolver domain
          case errOrIps of
            Left err -> dnsError err
            Right actualIpv4s -> expectedIpv4s `shouldBe` actualIpv4s
        CheckAAAA domain expectedIpv6s -> domainIt domain "AAAA" $ \resolver -> do
          errOrIps <- retryDNS $ DNS.lookupAAAA resolver domain
          case errOrIps of
            Left err -> dnsError err
            Right actualIpv6s -> expectedIpv6s `shouldBe` actualIpv6s
        CheckMX domain expectedDomains -> domainIt domain "MX" $ \resolver -> do
          errOrDomains <- retryDNS $ DNS.lookupMX resolver domain
          case errOrDomains of
            Left err -> dnsError err
            Right actualDomains ->
              sort expectedDomains `shouldBe` sort actualDomains
        CheckTXT domain expectedValues -> domainIt domain "TXT" $ \resolver -> do
          errOrValues <- retryDNS $ DNS.lookupTXT resolver domain
          case errOrValues of
            Left err -> dnsError err
            Right actualValues ->
              expectedValues `shouldBe` actualValues
        CheckCNAME domain expectedValues -> domainIt domain "CNAME" $ \resolver -> do
          errOrDNSMessage <- retryDNS $ DNS.lookupRaw resolver domain CNAME
          case errOrDNSMessage >>= (`fromDNSMessage` parseCNAMEDNSMessage) of
            Left err -> dnsError err
            Right actualValues ->
              expectedValues `shouldBe` actualValues
        CheckNS domain expectedValues -> domainIt domain "NS" $ \resolver -> do
          errOrValues <- retryDNS $ DNS.lookupNS resolver domain
          case errOrValues of
            Left err -> dnsError err
            Right actualValues -> sort expectedValues `shouldBe` sort actualValues

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

data CheckSpec
  = CheckSpec
      { specRetryPolicy :: !RetryPolicySpec,
        specChecks :: ![Check]
      }
  deriving (Show, Eq, Generic)

instance FromJSON CheckSpec where
  parseJSON = viaYamlSchema

instance YamlSchema CheckSpec where
  yamlSchema =
    objectParser "CheckSpec" $
      CheckSpec
        <$> optionalFieldWithDefault "retry-policy" defaultRetryPolicySpec "The retry policy for flaky checks due to network failures etc"
        <*> requiredField "checks" "The checks to perform"

data RetryPolicySpec
  = RetryPolicySpec
      { retryPolicySpecMaxRetries :: !Word,
        retryPolicySpecBaseDelay :: !Word
      }
  deriving (Show, Eq, Generic)

instance FromJSON RetryPolicySpec where
  parseJSON = viaYamlSchema

instance YamlSchema RetryPolicySpec where
  yamlSchema =
    objectParser "RetryPolicySpec" $
      RetryPolicySpec
        <$> optionalFieldWithDefault "max-retries" (retryPolicySpecMaxRetries defaultRetryPolicySpec) "The maximum number of retries"
        <*> optionalFieldWithDefault "base-delay" (retryPolicySpecBaseDelay defaultRetryPolicySpec) "The delay between the first and second try, in microseconds"

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
  = CheckA !Domain ![IPv4]
  | CheckAAAA !Domain ![IPv6]
  | CheckMX !Domain ![(Domain, Int)]
  | CheckTXT !Domain ![ByteString]
  | CheckCNAME !Domain ![Domain]
  | CheckNS !Domain ![Domain]
  deriving (Show, Eq, Generic)

instance FromJSON IPv4 where
  parseJSON = fmap read . parseJSON -- TODO nicer failures using readMaybe

instance FromJSON IPv6 where
  parseJSON = fmap read . parseJSON -- TODO nicer failures using readMaybe

instance FromJSON Check where
  parseJSON = viaYamlSchema

instance YamlSchema Check where
  yamlSchema =
    objectParser "Check" $
      alternatives
        [ (CheckA <$ typeField "a")
            <*> domainField
            <*> singleOrListFieldWith
              "ip"
              "ips"
              viaRead,
          (CheckAAAA <$ typeField "aaaa")
            <*> domainField
            <*> singleOrListFieldWith "ip" "ips" viaRead,
          (CheckMX <$ typeField "mx")
            <*> domainField
            <*> singleOrListFieldWith "value" "values" (eitherParser parseMXValue yamlSchema),
          (CheckTXT <$ typeField "txt")
            <*> domainField
            <*> singleOrListFieldWith "value" "values" (parseTXTValue <$> yamlSchema),
          (CheckCNAME <$ typeField "cname")
            <*> domainField
            <*> singleOrListFieldWith "value" "values" (parseCNAMEValue <$> yamlSchema),
          (CheckNS <$ typeField "ns")
            <*> domainField
            <*> singleOrListFieldWith "value" "values" (parseDomain <$> yamlSchema)
        ]

typeField :: Text -> ObjectParser Text
typeField typeName = requiredFieldWith' "type" (literalString typeName)

domainField :: ObjectParser Domain
domainField = requiredFieldWith "domain" "The domain" (parseDomain <$> yamlSchema)

singleOrListFieldWith :: Text -> Text -> YamlParser a -> ObjectParser [a]
singleOrListFieldWith singleKey listKey par =
  alternatives
    [ (: []) <$> requiredFieldWith' singleKey par,
      requiredFieldWith' listKey (toList <$> ParseArray Nothing (ParseList par))
    ]

parseMXValue :: Text -> Either String (Domain, Int)
parseMXValue t = case T.words t of
  [pt, dt] -> do
    let domain = parseDomain dt
    case readMaybe (T.unpack pt) of
      Nothing -> Left $ unwords ["Failed to parse MX record priority:", show pt]
      Just priority -> pure (domain, priority)
  _ -> Left "Could not parse MX value"

parseDomain :: Text -> Domain
parseDomain = DNS.normalize . TE.encodeUtf8

parseTXTValue :: Text -> ByteString
parseTXTValue = TE.encodeUtf8

parseCNAMEValue :: Text -> ByteString
parseCNAMEValue = TE.encodeUtf8
