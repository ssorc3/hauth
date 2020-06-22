module Adapter.Redis.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Text.StringRandom
import Data.Has
import qualified Database.Redis as R

type State = R.Connection

withState :: String -> (State -> IO a) -> IO a
withState connUrl action = 
  case R.parseConnectInfo connUrl of
   Left _ -> throwString "Invalid Redis conn URL"
   Right connInfo
       -> do conn <- R.checkedConnect connInfo
             action conn
             
type Redis r m = (Has State r, MonadReader r m, MonadIO m)

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action
  
newSession :: Redis r m => D.UserId -> m D.SessionId
newSession uId = do
  sId <- liftIO $ stringRandomIO "[A-Za-z0-9]{32}"
  result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ uId)
  case result of
    Right R.Ok -> return sId
    err -> throwString $ "Unexpected redis error: " <> show err

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  result <- withConn $ R.get (encodeUtf8 sId)
  case result of
    Right (Just uIdStr) -> return $ readMay . unpack . decodeUtf8 $ uIdStr
    err -> throwString $ "Unexpected redis error: " <> show err
