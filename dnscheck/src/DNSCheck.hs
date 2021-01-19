{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNSCheck
  ( dnsCheck,
  )
where

import Control.Applicative
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import Data.IP
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml
import GHC.Generics (Generic)
import Network.DNS
import Network.DNS.Lookup as DNS
import Network.DNS.LookupRaw as DNS
import Network.DNS.Utils as DNS
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
      spec <- decodeFileThrow sfp
      runCheckSpec spec

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
    $ mapM_ singleCheckSpec specChecks

singleCheckSpec :: Check -> TestDef '[Resolver] ()
singleCheckSpec =
  let domainIt :: Domain -> String -> (Resolver -> IO ()) -> TestDef '[Resolver] ()
      domainIt d s = itWithOuter (unwords [s, show d])
      dnsError err = expectationFailure (show err)
   in \case
        CheckA domain expectedIpv4s -> domainIt domain "A" $ \resolver -> do
          errOrIps <- DNS.lookupA resolver domain
          case errOrIps of
            Left err -> dnsError err
            Right actualIpv4s -> expectedIpv4s `shouldBe` actualIpv4s
        CheckAAAA domain expectedIpv6s -> domainIt domain "AAAA" $ \resolver -> do
          errOrIps <- DNS.lookupAAAA resolver domain
          case errOrIps of
            Left err -> dnsError err
            Right actualIpv6s -> expectedIpv6s `shouldBe` actualIpv6s
        CheckMX domain expectedDomains -> domainIt domain "MX" $ \resolver -> do
          errOrDomains <- DNS.lookupMX resolver domain
          case errOrDomains of
            Left err -> dnsError err
            Right actualDomains ->
              expectedDomains `shouldBe` actualDomains
        CheckTXT domain expectedValues -> domainIt domain "TXT" $ \resolver -> do
          errOrValues <- DNS.lookupTXT resolver domain
          case errOrValues of
            Left err -> dnsError err
            Right actualValues ->
              expectedValues `shouldBe` actualValues
        CheckCNAME domain expectedValues -> domainIt domain "CNAME" $ \resolver -> do
          errOrDNSMessage <- DNS.lookupRaw resolver domain CNAME
          case errOrDNSMessage >>= (`fromDNSMessage` parseCNAMEDNSMessage) of
            Left err -> dnsError err
            Right actualValues ->
              expectedValues `shouldBe` actualValues
        CheckNS domain expectedValues -> domainIt domain "NS" $ \resolver -> do
          errOrValues <- DNS.lookupNS resolver domain
          case errOrValues of
            Left err -> dnsError err
            Right actualValues -> sort expectedValues `shouldBe` sort actualValues

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
      { specChecks :: ![Check]
      }
  deriving (Show, Eq, Generic)

instance FromJSON CheckSpec where
  parseJSON = withObject "CheckSpec" $ \o ->
    CheckSpec <$> o .: "checks"

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
  parseJSON = withObject "Check" $ \o -> do
    t <- o .: "type"
    domain <- parseDomain =<< o .: "domain"
    case (t :: Text) of
      "a" -> CheckA domain <$> singleOrList o "ip" "ips"
      "aaaa" -> CheckAAAA domain <$> singleOrList o "ip" "ips"
      "mx" -> CheckMX domain <$> (singleOrList o "value" "values" >>= parseMXValues)
      "txt" -> CheckTXT domain <$> (singleOrList o "value" "values" >>= parseTXTValues)
      "cname" -> CheckCNAME domain <$> (singleOrList o "value" "values" >>= parseCNAMEValues)
      "ns" -> CheckNS domain <$> (singleOrList o "value" "values" >>= mapM parseDomain)
      _ -> fail $ "Unknown dns record type: " <> T.unpack t

singleOrList :: FromJSON a => Object -> Text -> Text -> Parser [a]
singleOrList o singularKey pluralKey = (: []) <$> (o .: singularKey) <|> (o .: pluralKey)

parseMXValues :: [Text] -> Parser [(Domain, Int)]
parseMXValues = mapM parseMXValue

parseMXValue :: Text -> Parser (Domain, Int)
parseMXValue t = case T.words t of
  [pt, dt] -> do
    domain <- parseDomain dt
    case readMaybe (T.unpack pt) of
      Nothing -> fail $ unwords ["Failed to parse MX record priority:", show pt]
      Just priority -> pure (domain, priority)
  _ -> fail "Could not parse MX value"

parseDomain :: Text -> Parser Domain
parseDomain = pure . DNS.normalize . TE.encodeUtf8

parseTXTValues :: [Text] -> Parser [ByteString]
parseTXTValues = pure . map TE.encodeUtf8

parseCNAMEValues :: [Text] -> Parser [ByteString]
parseCNAMEValues = pure . map TE.encodeUtf8
