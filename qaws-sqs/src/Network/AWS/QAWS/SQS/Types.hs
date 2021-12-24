{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.QAWS.SQS.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Network.AWS as AWS
import RIO

newtype QueueUrl = QueueUrl {_unQueueUrl :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''QueueUrl

newtype WaitTime = WaitTime {_unWaitTime :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''WaitTime

newtype MessageLimit = MessageLimit {_unMessageLimit :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''MessageLimit

newtype ReceiptHandle = ReceiptHandle {_unReceiptHandle :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''ReceiptHandle

newtype MessageId = MessageId {_unMessageId :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''MessageId

newtype ARN = ARN {_unARN :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''ARN

newtype MessageCount = MessageCount {_unMessageCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''MessageCount

newtype DelayedMessageCount = DelayedMessageCount {_unDelayedMessageCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''DelayedMessageCount

newtype NotVisibleCount = NotVisibleCount {_unNotVisibleCount :: Int}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''NotVisibleCount

-- | A message that is sent to a queue. In contrast to the standard message type in the amazonka
-- libraries, this asserts that message id, receipt handle and body are all present.
data SQSMessage a = SQSMessage
  { _sqsMessageBody :: a,
    _sqsMessageMessageId :: MessageId,
    _sqsMessageReceiptHandle :: ReceiptHandle
  }
  deriving (Eq, Show)

makeLenses ''SQSMessage

data ReceiveMessageError
  = ReceiveMessageAWSError AWS.Error
  | ReceiveMessageDecodingError String
  | ReceiveMessageNoBody
  | ReceiveMessageNoReceiptHandle
  | ReceiveMessageNoMessageId
  deriving (Show)

instance Exception ReceiveMessageError

data QueueAttributes = QueueAttributes
  { queueAttributesArn :: !(Maybe Text),
    queueAttributesUrl :: !QueueUrl,
    queueAttributesMessages :: !(Maybe MessageCount),
    queueAttributesDelayedMessages :: !(Maybe DelayedMessageCount),
    queueAttributesNotVisibleMessages :: !(Maybe NotVisibleCount)
  }
  deriving (Eq, Show)
