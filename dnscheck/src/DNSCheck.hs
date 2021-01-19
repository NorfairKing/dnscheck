{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DNSCheck
  ( dnsCheck,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Functor.Identity
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
import Rainbow
import System.Environment
import System.Exit
import Text.Read

dnsCheck :: IO ()
dnsCheck = do
  args <- getArgs
  case args of
    [] -> die "Supply a spec file path as an argument"
    (sfp : _) -> do
      spec <- decodeFileThrow sfp
      checkSpec spec

checkSpec :: Spec -> IO ()
checkSpec Spec {..} = do
  rs <- makeResolvSeed defaultResolvConf
  ec <- withResolver rs $ \resolver -> do
    results <- mapM (checkCheck resolver) specChecks
    let (ec, chunkss) = runWriter (execStateT (resultsReport results) ExitSuccess)
    byteStringMaker <- byteStringMakerFromEnvironment
    let bytestrings = map (chunksToByteStrings byteStringMaker) chunkss
    forM_ bytestrings $ \bs -> do
      mapM SB.putStr bs
      SB8.putStrLn ""
    pure ec
  exitWith ec

type Report = ReportM ()

type ReportM a = StateT ExitCode (Writer [[Chunk]]) a

resultsReport :: [CheckResult] -> Report
resultsReport = mapM_ resultReport

resultReport :: CheckResult -> Report
resultReport cr = do
  case cr of
    ResultOk {} -> pure ()
    _ -> put (ExitFailure 1)
  let line :: [Chunk] -> Report
      line cs = tell [intersperse (chunk " ") cs]
      domainChunk = fore blue . chunk . TE.decodeLatin1
      errorChunk = fore red . chunk . T.pack . show
      showChunk :: Show a => a -> Chunk
      showChunk = chunk . T.pack . show
  case cr of
    ResultError domain err ->
      line [domainChunk domain, errorChunk err]
    ResultAFailed domain expected actual ->
      line [domainChunk domain, showChunk expected, showChunk actual]
    ResultAAAAFailed domain expected actual ->
      line [domainChunk domain, showChunk expected, showChunk actual]
    ResultMXFailed domain expected actual ->
      line [domainChunk domain, showChunk expected, showChunk actual]
    ResultTXTFailed domain expected actual ->
      line [domainChunk domain, showChunk expected, showChunk actual]
    ResultCNAMEFailed domain expected actual ->
      line [domainChunk domain, showChunk expected, showChunk actual]
    ResultNSFailed domain expected actual ->
      line [domainChunk domain, showChunk expected, showChunk actual]
    ResultOk domain -> line [domainChunk domain, fore green $ chunk "OK"]

checkCheck :: Resolver -> Check -> IO CheckResult
checkCheck resolver c =
  case c of
    CheckA domain expectedIpv4s -> do
      errOrIps <- DNS.lookupA resolver domain
      pure $ case errOrIps of
        Left err -> ResultError domain err
        Right actualIpv4s ->
          if expectedIpv4s == actualIpv4s
            then ResultOk domain
            else ResultAFailed domain expectedIpv4s actualIpv4s
    CheckAAAA domain expectedIpv6s -> do
      errOrIps <- DNS.lookupAAAA resolver domain
      pure $ case errOrIps of
        Left err -> ResultError domain err
        Right actualIpv6s ->
          if expectedIpv6s == actualIpv6s
            then ResultOk domain
            else ResultAAAAFailed domain expectedIpv6s actualIpv6s
    CheckMX domain expectedDomains -> do
      errOrDomains <- DNS.lookupMX resolver domain
      pure $ case errOrDomains of
        Left err -> ResultError domain err
        Right actualDomains ->
          if expectedDomains == actualDomains
            then ResultOk domain
            else ResultMXFailed domain expectedDomains actualDomains
    CheckTXT domain expectedValues -> do
      errOrValues <- DNS.lookupTXT resolver domain
      pure $ case errOrValues of
        Left err -> ResultError domain err
        Right actualValues ->
          if expectedValues == actualValues
            then ResultOk domain
            else ResultTXTFailed domain expectedValues actualValues
    CheckCNAME domain expectedValues -> do
      errOrDNSMessage <- DNS.lookupRaw resolver domain CNAME
      pure $ case errOrDNSMessage >>= (`fromDNSMessage` parseCNAMEDNSMessage) of
        Left err -> ResultError domain err
        Right actualValues ->
          if expectedValues == actualValues
            then ResultOk domain
            else ResultCNAMEFailed domain expectedValues actualValues
    CheckNS domain expectedValues -> do
      errOrValues <- DNS.lookupNS resolver domain
      pure $ case errOrValues of
        Left err -> ResultError domain err
        Right actualValues ->
          if sort expectedValues == sort actualValues
            then ResultOk domain
            else ResultNSFailed domain expectedValues actualValues

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

data CheckResult
  = ResultError !Domain !DNSError
  | ResultAFailed !Domain ![IPv4] ![IPv4]
  | ResultAAAAFailed !Domain ![IPv6] ![IPv6]
  | ResultMXFailed !Domain ![(Domain, Int)] ![(Domain, Int)]
  | ResultTXTFailed !Domain ![ByteString] ![ByteString]
  | ResultCNAMEFailed !Domain ![Domain] ![Domain]
  | ResultNSFailed !Domain ![Domain] ![Domain]
  | ResultOk !Domain
  deriving (Show, Eq, Generic)

data Spec = Spec
  { specChecks :: ![Check]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Spec where
  parseJSON = withObject "Spec" $ \o ->
    Spec <$> o .: "checks"

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
