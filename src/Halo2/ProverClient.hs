{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Halo2.ProverClient
  ( Port (Port),
    mockProve,
    prove,
    buildProver,
    runProver,
    callMockProver,
    callProver,
  )
where

import Cast (integerToWord8)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (cancel, withAsync)
import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Halo2.Circuit (getCellMapColumns)
import Halo2.Codegen (generateProject)
import Halo2.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import Halo2.Types.CellReference (CellReference)
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory))
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutNone)
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import Servant.API (JSON, PlainText, OctetStream, Post, ReqBody, (:>))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, ClientM, Scheme (Http), client, mkClientEnv, runClientM)
import Stark.Types.Scalar (Scalar)
import Turtle (ExitCode (ExitSuccess), empty, shell)

newtype Port = Port Int

instance ToJSON ByteString where
  toJSON = toJSON . BS.unpack

-- Generate and build the Halo2 prover and use it to run the
-- mock prover API.
-- Must be supplied with a target directory in which to generate
-- and build the prover.
-- Must be supplied with with a base URL at which to run the
-- prover server.
mockProve ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError (ErrorMessage ()) m
  ) =>
  ArithmeticCircuit ->
  Argument ->
  TargetDirectory ->
  Port ->
  m ()
mockProve c arg target (Port port) = do
  buildProver c target
  withAsync (runProver target (Port port)) $ \server -> do
    liftIO $ threadDelay 5000000
    mgr <- liftIO $ newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutNone}
    callMockProver (mkClientEnv mgr baseUrl) arg
    cancel server
  where
    baseUrl = BaseUrl Http "127.0.0.1" port ""

-- Generate and build the Halo2 prover and use it to run the
-- real prover API. Also verifies the proof for good measure.
prove ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError (ErrorMessage ()) m
  ) =>
  ArithmeticCircuit ->
  Argument ->
  TargetDirectory ->
  Port ->
  m ByteString
prove c arg target (Port port) = do
  buildProver c target
  withAsync (runProver target (Port port)) $ \server -> do
    liftIO $ threadDelay 5000000
    mgr <- liftIO $ newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutNone}
    let env = mkClientEnv mgr baseUrl
    proof <- callProver env arg
    callVerifier env (arg ^. #statement) proof
    cancel server
    return proof
  where
    baseUrl = BaseUrl Http "127.0.0.1" port ""

-- Generate and build the Halo2 prover.
buildProver ::
  (MonadIO m, MonadError (ErrorMessage ()) m) =>
  ArithmeticCircuit ->
  TargetDirectory ->
  m ()
buildProver c (TargetDirectory target) = do
  void . liftIO $ shell ("rm -rf " <> pack target) empty
  liftIO $ generateProject (TargetDirectory target) c
  res <- liftIO $ shell ("cd " <> pack target <> "; nix develop --command cargo build") empty
  when (res /= ExitSuccess) . throwError $
    ErrorMessage () "nix develop --command cargo build: was not successful"

-- Start the Halo2 prover server.
runProver ::
  ( MonadIO m,
    MonadBase IO m,
    MonadError (ErrorMessage ()) m
  ) =>
  TargetDirectory ->
  Port ->
  m ()
runProver (TargetDirectory target) (Port port) = do
  res <- liftIO $ shell ("cd " <> pack target <> "; nix develop --command cargo run " <> pack (show port)) empty
  when (res /= ExitSuccess) . throwError $
    ErrorMessage () "nix develop --command cargo run: was not successful"

type MockProverApi = "mock_prove" :> ReqBody '[JSON] EncodedArgument :> Post '[PlainText] Text

type ProverApi = "prove" :> ReqBody '[JSON] EncodedArgument :> Post '[OctetStream] ByteString

type VerifierApi = "verify"
       :> ReqBody '[JSON] (Map ColumnIndex [[[Word8]]], ByteString)
       :> Post '[OctetStream] ByteString

data EncodedArgument = EncodedArgument
  { instance_data :: Map ColumnIndex [[[Word8]]],
    advice_data :: Map ColumnIndex [[[Word8]]]
  }
  deriving (Eq, Ord, Generic)

instance ToJSON EncodedArgument

instance FromJSON EncodedArgument

-- Turn a scalar into a little-endian byte string and group into groups of eight.
encodeScalar :: Scalar -> [[Word8]]
encodeScalar =
  unfoldr (\xs -> if null xs then Nothing else pure (splitAt 8 xs))
    . encodeScalarBytesLE

-- Turn a scalar into a little-endian byte string.
encodeScalarBytesLE :: Scalar -> [Word8]
encodeScalarBytesLE =
  take 64
    . unfoldr
      ( \x ->
          let (a, b) = x `divMod` 256
           in -- a = x / 256
              -- b = x mod 256
              (,a) <$> integerToWord8 b
      )
    . (^. #unScalar)

encodeArgumentData :: Map CellReference Scalar -> Map ColumnIndex [[[Word8]]]
encodeArgumentData = fmap (fmap encodeScalar . Map.elems) . getCellMapColumns

encodeArgument :: Argument -> EncodedArgument
encodeArgument (Argument (Statement s) (Witness w)) =
  EncodedArgument
    (encodeArgumentData s)
    (encodeArgumentData w)

mockProverClient :: EncodedArgument -> ClientM ()
mockProverClient = void . client (Proxy @MockProverApi)

proverClient :: EncodedArgument -> ClientM ByteString
proverClient = client (Proxy @ProverApi)

verifierClient :: Map ColumnIndex [[[Word8]]] -> ByteString -> ClientM ()
verifierClient stmt proof = void $ client (Proxy @VerifierApi) (stmt, proof)

-- Call the mock prover API on a running instance of the
-- Halo2 prover server.
callMockProver ::
  ( MonadIO m,
    MonadError (ErrorMessage ()) m
  ) =>
  ClientEnv ->
  Argument ->
  m ()
callMockProver env arg = do
  res <- liftIO $ runClientM (mockProverClient (encodeArgument arg)) env
  case res of
    Left err -> throwError (ErrorMessage () ("mock prover returned error: " <> pack (show err)))
    Right _ -> pure ()

-- Call the real prover API on a running instance of the
-- Halo2 prover server.
callProver ::
  ( MonadIO m,
    MonadError (ErrorMessage ()) m
  ) =>
  ClientEnv ->
  Argument ->
  m ByteString
callProver env arg = do
  res <- liftIO $ runClientM (proverClient (encodeArgument arg)) env
  case res of
    Left err -> throwError (ErrorMessage () ("real prover returned error: " <> pack (show err)))
    Right proof -> pure proof

-- Call the verifier API on a running instance of the Halo2
-- prover server.
callVerifier ::
  ( MonadIO m,
    MonadError (ErrorMessage ()) m
  ) =>
  ClientEnv ->
  Statement ->
  ByteString ->
  m ()
callVerifier env stmt proof = do
  res <- liftIO $ runClientM (verifierClient (encodeArgumentData (stmt ^. #unStatement)) proof) env
  case res of
    Left _err -> throwError $ ErrorMessage () "verifier returned error"
    Right () -> pure ()
