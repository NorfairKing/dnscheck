{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DNSCheck
  ( dnsCheck,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import Data.IP
import Data.Text (Text)
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
checkCheck = \case
  CheckA domain expectedIpv4 -> do
    rs <- makeResolvSeed defaultResolvConf
    withResolver rs $ \resolver -> do
      errOrIps <- DNS.lookupA resolver domain
      pure $ case errOrIps of
        Left err -> ResultError err
        Right ips ->
          if expectedIpv4 `elem` ips
            then ResultOk
            else ResultAFailed domain expectedIpv4 ips

data CheckResult
  = ResultError DNSError
  | ResultAFailed Domain IPv4 [IPv4]
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
  deriving (Show, Eq, Generic)

instance FromJSON Domain where
  parseJSON = fmap SB8.pack . parseJSON

instance FromJSON IPv4 where
  parseJSON = fmap read . parseJSON

instance FromJSON Check where
  parseJSON = withObject "Check" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "a" -> CheckA <$> o .: "domain" <*> o .: "ip"
