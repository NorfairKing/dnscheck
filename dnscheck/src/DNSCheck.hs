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
import qualified Data.ByteString.Char8 as SB8
import Data.IP
import Data.List
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
          if sort expectedIpv4s == sort actualIpv4s
            then ResultOk domain
            else ResultAFailed domain expectedIpv4s actualIpv4s
    CheckAAAA domain expectedIpv6s -> do
      errOrIps <- DNS.lookupAAAA resolver domain
      pure $ case errOrIps of
        Left err -> ResultError domain err
        Right actualIpv6s ->
          if sort expectedIpv6s == sort actualIpv6s
            then ResultOk domain
            else ResultAAAAFailed domain expectedIpv6s actualIpv6s

data CheckResult
  = ResultError Domain DNSError
  | ResultAFailed Domain [IPv4] [IPv4]
  | ResultAAAAFailed Domain [IPv6] [IPv6]
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
      "a" -> CheckA <$> o .: "domain" <*> singleOrList o "ip"
      "aaaa" -> CheckAAAA <$> o .: "domain" <*> singleOrList o "ip"
      _ -> fail $ "Unknown dns record type: " <> T.unpack t

singleOrList :: FromJSON a => Object -> Text -> Parser [a]
singleOrList o k = (: []) <$> (o .: k) <|> (o .: k)
