module Adapter.RabbitMQ.Auth where

import qualified Adapter.InMemory.Auth as M
import Adapter.RabbitMQ.Common
import ClassyPrelude
import Data.Aeson
import Data.Aeson.TH
import qualified Domain.Auth as D
import Katip
import Network.AMQP

data EmailVerificationPayload
  = EmailVerificationPayload
      { emailVerificationPayloadEmail :: Text,
        emailVerificationPayloadVerificationCode :: Text
      }

$( let structName = fromMaybe "" . lastMay . splitElem '.' . show $ "EmailVerificationPayload"
       lowercaseFirst (x : xs) = toLower [x] <> xs
       lowercaseFirst xs = xs
       options =
         defaultOptions
           { fieldLabelModifier = lowercaseFirst . drop (length structName)
           }
    in deriveJSON options ''EmailVerificationPayload
 )

notifyEmailVerification ::
  (Rabbit r m) =>
  D.Email ->
  D.VerificationCode ->
  m ()
notifyEmailVerification email vCode =
  let payload = EmailVerificationPayload (D.rawEmail email) vCode
   in publish "auth" "userRegistered" payload

consumeEmailVerification ::
  (M.InMemory r m, KatipContext m, MonadUnliftIO m) =>
  (m Bool -> IO Bool) ->
  Message ->
  IO Bool
consumeEmailVerification runner msg =
  runner $ consumeAndProcess msg handler
  where
    handler payload =
      case D.mkEmail (emailVerificationPayloadEmail payload) of
        Left err -> withMsgAndErr msg err $ do
          $(logTM) ErrorS "Email format is invalid. Rejecting."
          return False
        Right email -> do
          let vCode = emailVerificationPayloadVerificationCode payload
          M.notifyEmailVerification email vCode
          return True

init :: (M.InMemory r m, KatipContext m, MonadUnliftIO m)
     => State -> (m Bool -> IO Bool) -> IO ()
init state runner = do
  initQueue state "verifyEmail" "auth" "userRegistered"
  initConsumer state "verifyEmail" (consumeEmailVerification runner)
