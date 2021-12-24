{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import qualified Network.AWS as AWS
import qualified Network.AWS.Auth as AWS
import RIO
import qualified RIO.Directory as Directory
import qualified RIO.Text as Text
import qualified System.Environment as Environment

newtype EnvironmentFile = EnvironmentFile {_unEnvironmentFile :: FilePath}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''EnvironmentFile

data LoadEnvironmentError
  = EnvironmentFileNotFound EnvironmentFile
  | AWSAuthError AWS.AuthError
  deriving (Show)

instance Exception LoadEnvironmentError

loadAWSEnvironment :: EnvironmentFile -> IO (Either LoadEnvironmentError AWS.Env)
loadAWSEnvironment environmentFile = do
  loadEnvResult <- loadEnvFile environmentFile
  case loadEnvResult of
    Left e -> pure $ Left e
    Right () -> do
      envResult <- mapLeft AWSAuthError <$> try (AWS.newEnv AWS.Discover)
      case envResult of
        err@(Left _) -> pure err
        Right e -> pure $ Right e

tryRunAWS ::
  (MonadReader env m, AWS.AWSRequest a, AWS.HasEnv env, MonadUnliftIO m) =>
  a ->
  m (Either AWS.Error (AWS.Rs a))
tryRunAWS = runAWS >>> try

runAWS ::
  (MonadReader env m, AWS.AWSRequest a, AWS.HasEnv env, MonadUnliftIO m) =>
  a ->
  m (AWS.Rs a)
runAWS action = do
  awsEnvironment <- view AWS.environment
  AWS.runResourceT $ AWS.runAWS awsEnvironment $ AWS.send action

tryRunAWS' ::
  (AWS.AWSRequest a, MonadUnliftIO m) =>
  AWS.Env ->
  a ->
  m (Either AWS.Error (AWS.Rs a))
tryRunAWS' env = runAWS' env >>> try

runAWS' ::
  (AWS.AWSRequest a, MonadUnliftIO m) =>
  AWS.Env ->
  a ->
  m (AWS.Rs a)
runAWS' env action = AWS.runResourceT $ AWS.runAWS env $ AWS.send action

-- | Loads a `.env` file if it's available, changing the current environment.
loadEnvFile :: EnvironmentFile -> IO (Either LoadEnvironmentError ())
loadEnvFile ef@(EnvironmentFile path) = do
  dotEnvExists <- Directory.doesFileExist path
  if dotEnvExists
    then do
      dotEnvValues <- parseDotEnv path
      forM_ dotEnvValues $ \(key, value) -> do
        Environment.setEnv key value
      pure $ Right ()
    else pure $ Left $ EnvironmentFileNotFound ef

-- | Parses a `.env` file into a list of key value pairs.
parseDotEnv :: FilePath -> IO [(String, String)]
parseDotEnv filePath = do
  ( Text.lines
      >>> fmap Text.strip
      >>> filter (\l -> l /= "" && not (Text.isPrefixOf "#" l))
      >>> fmap (Text.break (== '='))
      >>> fmap (bimap sanitizeKey sanitizeValue)
    )
    <$> readFileUtf8 filePath
  where
    sanitizeKey :: Text -> String
    sanitizeKey = Text.dropWhile (`elem` [' ', '#']) >>> Text.unpack

    sanitizeValue :: Text -> String
    sanitizeValue = Text.dropWhile (== '=') >>> Text.filter (/= '"') >>> Text.unpack
