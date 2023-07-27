{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Halo2.ProverClient
  ( Port (Port),
    mockProve,
    buildProver,
    runProver,
    callMockProver
  ) where


import Cast (integerToWord8)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (cancel, withAsync)
import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (ToJSON, FromJSON)
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
import Network.HTTP.Client (newManager, defaultManagerSettings, managerResponseTimeout, responseTimeoutNone)
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import Servant.API ((:>), ReqBody, JSON, PlainText, Post)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, Scheme (Http), ClientM, client, mkClientEnv, runClientM)
import Stark.Types.Scalar (Scalar)
import Turtle (shell, empty, ExitCode (ExitSuccess))


newtype Port = Port Int


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
    mgr <- liftIO $ newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutNone }
    callMockProver (mkClientEnv mgr baseUrl) arg
    cancel server
  where
    baseUrl = BaseUrl Http "127.0.0.1" port ""


-- Generate and build the Halo2 prover.
buildProver ::
  ( MonadIO m, MonadError (ErrorMessage ()) m ) =>
  ArithmeticCircuit ->
  TargetDirectory ->
  m ()
buildProver c (TargetDirectory target) = do
  void . liftIO $ shell ("rm -rf " <> pack target) empty
  liftIO $ generateProject (TargetDirectory target) c
  res <- liftIO $ shell ("cd " <> pack target <> "; nix develop --command cargo build") empty
  when (res /= ExitSuccess) . throwError
    $ ErrorMessage () "nix develop --command cargo build: was not successful"


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
  when (res /= ExitSuccess) . throwError
    $ ErrorMessage () "nix develop --command cargo run: was not successful"


type ProverApi = "mock_prove" :> ReqBody '[JSON] EncodedArgument :> Post '[PlainText] Text


data EncodedArgument =
  EncodedArgument
    { instance_data :: Map ColumnIndex [[[Word8]]],
      advice_data :: Map ColumnIndex [[[Word8]]]
    }
  deriving (Eq, Ord, Generic)

instance ToJSON EncodedArgument
instance FromJSON EncodedArgument


-- Turn a scalar into a little-endian byte string and group into groups of eight.
encodeScalar :: Scalar -> [[Word8]]
encodeScalar =
  unfoldr (\xs -> if null xs then Nothing else pure (take 8 xs, drop 8 xs))
    . encodeScalarBytesLE


-- Turn a scalar into a little-endian byte string.
encodeScalarBytesLE :: Scalar -> [Word8]
encodeScalarBytesLE =
  take 64
    . unfoldr
      (\x -> let (a, b) = x `divMod` 256
               -- a = x / 256
               -- b = x mod 256
             in (,a) <$> integerToWord8 b)
    . (^. #unScalar)


encodeArgumentData :: Map CellReference Scalar -> Map ColumnIndex [[[Word8]]]
encodeArgumentData = fmap (fmap encodeScalar . Map.elems) . getCellMapColumns


encodeArgument :: Argument -> EncodedArgument
encodeArgument (Argument (Statement s) (Witness w)) =
  EncodedArgument
    (encodeArgumentData s)
    (encodeArgumentData w)


mockProverClient :: EncodedArgument -> ClientM ()
mockProverClient arg = void $ client (Proxy @ProverApi) arg


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
