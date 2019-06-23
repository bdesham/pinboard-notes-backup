module Utils.FriendlyReqError where

import Data.Text (Text, isInfixOf, pack)
import Network.HTTP.Client
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types.Status

friendlyReqError :: Req.HttpException -> Text

friendlyReqError (Req.VanillaHttpException (HttpExceptionRequest _ (StatusCodeException resp _)))
  | responseStatus resp == unauthorized401 = "Error: The username or API token was incorrect."
  | otherwise                              = "Error: Received an unexpected HTTP "
                                             <> pack (show (statusCode (responseStatus resp))) <>
                                             " response from the server."

friendlyReqError (Req.VanillaHttpException (HttpExceptionRequest _ ResponseTimeout)) =
    "Error: The server took too long to return a response."

friendlyReqError (Req.VanillaHttpException (HttpExceptionRequest _ ConnectionTimeout)) =
    "Error: Attempting to connect to the server timed out."

friendlyReqError (Req.VanillaHttpException (HttpExceptionRequest _ (ConnectionFailure exc)))
  | "nodename nor servname provided, or not known" `isInfixOf` (pack (show exc)) =
      "Error: The Pinboard server's IP address could not be looked up. Are\n\
	  \you sure your internet connection is active?"
  | otherwise = "Could not connect to the server: " <> pack (show exc)

friendlyReqError (Req.VanillaHttpException (HttpExceptionRequest _ other)) =
    "Error while communicating with the Pinboard server: " <> pack (show other)

friendlyReqError (Req.VanillaHttpException (InvalidUrlException url reason)) =
    "Error: The URL \"" <> pack url <> "\" is invalid: " <> pack reason

friendlyReqError (Req.JsonHttpException reason) = "Error decoding JSON: " <> pack reason
