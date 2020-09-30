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

import Control.Monad
import qualified Data.ByteString.Char8 as SB8
import Data.IP
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics (Generic)
import Network.DNS
import Network.DNS.Lookup as DNS
import System.Environment
import System.Exit

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
  results <- mapM checkCheck specChecks
  mapM_ print results
  unless (all (== ResultOk) results) $ die "Some check(s) failed"

checkCheck :: Check -> IO CheckResult
checkCheck c = do
  rs <- makeResolvSeed defaultResolvConf
  withResolver rs $ \resolver ->
    case c of
      CheckA domain expectedIpv4 -> do
        errOrIps <- DNS.lookupA resolver domain
        pure $ case errOrIps of
          Left err -> ResultError err
          Right ips ->
            if expectedIpv4 `elem` ips
              then ResultOk
              else ResultAFailed domain expectedIpv4 ips
      CheckAAAA domain expectedIpv6 -> do
        errOrIps <- DNS.lookupAAAA resolver domain
        pure $ case errOrIps of
          Left err -> ResultError err
          Right ips ->
            if expectedIpv6 `elem` ips
              then ResultOk
              else ResultAAAAFailed domain expectedIpv6 ips

data CheckResult
  = ResultError DNSError
  | ResultAFailed Domain IPv4 [IPv4]
  | ResultAAAAFailed Domain IPv6 [IPv6]
  | ResultOk
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
  = CheckA Domain IPv4
  | CheckAAAA Domain IPv6
  deriving (Show, Eq, Generic)

instance FromJSON Domain where
  parseJSON = fmap SB8.pack . parseJSON

instance FromJSON IPv4 where
  parseJSON = fmap read . parseJSON

instance FromJSON IPv6 where
  parseJSON = fmap read . parseJSON

instance FromJSON Check where
  parseJSON = withObject "Check" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "a" -> CheckA <$> o .: "domain" <*> o .: "ip"
      "aaaa" -> CheckAAAA <$> o .: "domain" <*> o .: "ip"
      _ -> fail $ "Unknown dns record type: " <> T.unpack t
