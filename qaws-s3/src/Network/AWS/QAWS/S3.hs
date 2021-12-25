-- | Has utility functions for dealing with Amazon's Simple Storage Service (S3).
module Network.AWS.QAWS.S3
  ( getFileStream,
    runWithFileStream,
    runWithFileStream',
    objectExists,
    objectExists',
  )
where

import Conduit (ConduitT, ResourceT, runConduitRes, (.|))
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Body as AWS
import Network.AWS.QAWS
import qualified Network.AWS.S3 as AWSS3
import RIO

objectExists ::
  (MonadUnliftIO m, MonadReader env m, AWS.HasEnv env) =>
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  m (Either AWS.Error Bool)
objectExists bucketName objectKey = do
  awsEnv <- view AWS.environment
  objectExists' awsEnv bucketName objectKey

objectExists' ::
  (MonadUnliftIO m) =>
  AWS.Env ->
  AWSS3.BucketName ->
  AWSS3.ObjectKey ->
  m (Either AWS.Error Bool)
objectExists' awsEnv bucket key = do
  let command = AWSS3.headObject bucket key
  either Left (((^. AWSS3.horsResponseStatus) >>> (== 200)) >>> Right) <$> tryRunAWS' awsEnv command

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
