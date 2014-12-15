{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy (ByteString)
import Control.Monad.Reader
import Control.Monad.State
import Network.HTTP.Types (Status, status200, ResponseHeaders)
import Network.Wai
import Network.Wai.Handler.Warp (run)

data RequestData = RequestData { waiReq :: Request }
data ResponseState = ResponseState { resStatus :: Status, resHeaders :: ResponseHeaders, resContent :: ByteString }


type Handler a = ReaderT RequestData (StateT ResponseState IO) a

runHandler :: Request -> Handler () -> IO ResponseState
runHandler req h = execStateT (runReaderT h (RequestData req)) defResponseState
  where
    defResponseState = ResponseState status200 [] ""

handlerToApp :: Handler () -> Application
handlerToApp h = \req respond -> do
    res <- runHandler req h
    respond $ responseLBS (resStatus res) (resHeaders res) (resContent res)
  where
    defResponseState = ResponseState status200 [] ""

app :: Application
app _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "hello"

main = run 3000 app
