{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Register (runApp, apiRegister) where

import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types.Status (badRequest400,internalServerError500)
import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text as T
import qualified Web.Scotty as S


apiRegister :: S.ScottyM ()
apiRegister = do
  S.get "/" showLandingPage
  S.get "/byEmails" byEmails
  S.post "/register" register
  --S.post "/register" registrationFailure

showLandingPage :: S.ActionM ()
showLandingPage = do
  -- show our developers.cj.com registration page
  S.setHeader "Content-Type" "text/html"
  S.file "index.html"

byEmails :: S.ActionM ()
byEmails = S.json $ object ["email" .= String "halarnold2000@yahoo.com", "password" .= String "fuzzle"]

-- this needs to happen for email@such.com:Z23IUms102
register :: S.ActionM ()
register = do

  emailAddress <- S.param "email"

  registered <- liftIO (registerInterest emailAddress )
  case registered of
    Just errorMessage -> do
      S.json $ object [ "error" .= errorMessage ]
      S.status internalServerError500

    Nothing -> do
      S.json $ object [ "ok" .= ("ok" :: String) ]

registerInterest :: String -> IO (Maybe String)
-- hit the db, here with the new insert
registerInterest "halarnold2000@yahoo.com" = putStrLn "Registered!" >> return Nothing
registerInterest _ = return (Just "Bad Request")

-- registrationFailure :: S.ActionM ()
-- registrationFailure = do
--   S.json $ object [ "error" .= ("Invalid request" :: String) ]
--   S.status badRequest400

runApp :: IO ()
runApp = S.scotty 8080 apiRegister
