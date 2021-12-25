-- | Has utility functions for dealing with Amazon's Simple Storage Service (S3).
module Network.AWS.QAWS.S3
  ( getFileStream,
    runWithFileStream,
    runWithFileStream',
  )
where

import Conduit (ConduitT, ResourceT, runConduitRes, (.|))
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Body as AWS
import qualified Network.AWS.S3 as AWSS3
import RIO

getFileStream ::
  (AWS.MonadAWS m) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  m (ConduitT () ByteString (ResourceT IO) ())
getFileStream bucket key = do
  let command = AWSS3.getObject bucket key
  ((^. AWSS3.gorsBody) >>> AWS._streamBody) <$> AWS.send command

runWithFileStream ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  ConduitT ByteString Void (ResourceT IO) a ->
  m (Either AWS.Error a)
runWithFileStream bucket key downstream = do
  awsEnv <- view AWS.environment
  runWithFileStream' awsEnv bucket key downstream

runWithFileStream' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  ConduitT ByteString Void (ResourceT IO) a ->
  m (Either AWS.Error a)
runWithFileStream' awsEnv bucket key downstream = do
  try $
    AWS.runResourceT $
      AWS.runAWS awsEnv $ do
        fileStream <- getFileStream bucket key
        liftIO $ runConduitRes $ fileStream .| downstream
