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
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import Data.IP
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml
import GHC.Generics (Generic)
import Network.DNS
import Network.DNS.Lookup as DNS
import Network.DNS.Utils as DNS
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
  withResolver rs $ \resolver -> do
    results <- mapM (checkCheck resolver) specChecks
    passeds <- printResults results
    unless (and passeds) exitFailure

printResults :: [CheckResult] -> IO [Bool]
printResults ls = forM ls $ \cr ->
  case cr of
    ResultError domain err -> do
      putStrLn $ unwords [SB8.unpack domain, show err]
      pure False
    ResultAFailed domain expected actual -> do
      putStrLn $ unwords [SB8.unpack domain, show expected, show actual]
      pure False
    ResultAAAAFailed domain expected actual -> do
      putStrLn $ unwords [SB8.unpack domain, show expected, show actual]
      pure False
    ResultMXFailed domain expected actual -> do
      putStrLn $ unwords [SB8.unpack domain, show expected, show actual]
      pure False
    ResultTXTFailed domain expected actual -> do
      putStrLn $ unwords [SB8.unpack domain, show expected, show actual]
      pure False
    ResultOk domain -> do
      putStrLn $ unwords [SB8.unpack domain, "OK"]
      pure True

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

data CheckResult
  = ResultError Domain DNSError
  | ResultAFailed Domain [IPv4] [IPv4]
  | ResultAAAAFailed Domain [IPv6] [IPv6]
  | ResultMXFailed Domain [(Domain, Int)] [(Domain, Int)]
  | ResultTXTFailed Domain [ByteString] [ByteString]
  | ResultOk Domain
  deriving (Show, Eq, Generic)

data Spec
  = Spec
      { specChecks :: [Check]
      }
  deriving (Show, Eq, Generic)

instance FromJSON Spec where
  parseJSON = withObject "Spec" $ \o ->
    Spec <$> o .: "checks"

data Check
  = CheckA Domain [IPv4]
  | CheckAAAA Domain [IPv6]
  | CheckMX Domain [(Domain, Int)]
  | CheckTXT Domain [ByteString]
  deriving (Show, Eq, Generic)

instance FromJSON IPv4 where
  parseJSON = fmap read . parseJSON -- TODO nicer failures using readMaybe

instance FromJSON IPv6 where
  parseJSON = fmap read . parseJSON -- TODO nicer failures using readMaybe

instance FromJSON Check where
  parseJSON = withObject "Check" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "a" -> CheckA <$> (parseDomain =<< o .: "domain") <*> singleOrList o "ip" "ips"
      "aaaa" -> CheckAAAA <$> (parseDomain =<< o .: "domain") <*> singleOrList o "ip" "ips"
      "mx" -> CheckMX <$> (parseDomain =<< o .: "domain") <*> (singleOrList o "value" "values" >>= parseMXValues)
      "txt" -> CheckTXT <$> (parseDomain =<< o .: "domain") <*> (singleOrList o "value" "values" >>= parseTXTValues)
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
