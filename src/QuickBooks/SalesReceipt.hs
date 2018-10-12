{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE PolyKinds         #-}
------------------------------------------------------------------------------
-- |
-- Module      : QuickBooks.Requests
-- Description :
-- Copyright   :
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
--
--
------------------------------------------------------------------------------

module QuickBooks.SalesReceipt
 ( createSalesReceiptRequest
--  , readSalesReceiptRequest
--  , updateSalesReceiptRequest
--  , deleteSalesReceiptRequest
--  , sendSalesReceiptRequest
 ) where

import qualified Network.OAuth.OAuth2      as OAuth2
import qualified Text.Email.Validate       as Email (EmailAddress, toByteString)

import           Data.ByteString.Char8
import           Data.Aeson                (encode, eitherDecode, object, Value(String, Null))
import           Data.String.Interpolate   (i)
import           Network.HTTP.Client       (httpLbs
                                           ,parseUrlThrow
                                           ,Request(..)
                                           ,RequestBody(..)
                                           ,Response(responseBody))
import           Network.HTTP.Types.Header (hAccept,hContentType)
import           Network.URI               ( escapeURIString
                                           , isUnescapedInURI)
import           URI.ByteString

import           QuickBooks.Authentication
import           QuickBooks.Types

import QuickBooks.Logging  (logAPICall)

-- | Create an sales receipt.
createSalesReceiptRequest :: APIEnv
                     => OAuthTokens
                     -> SalesReceipt
                     -> IO (Either String (QuickBooksResponse SalesReceipt))
createSalesReceiptRequest tok = postSalesReceipt tok

-- -- | Update an sales receipt.
-- updateSalesReceiptRequest :: APIEnv
--                      => OAuthTokens
--                      -> SalesReceipt
--                      -> IO (Either String (QuickBooksResponse SalesReceipt))
-- updateSalesReceiptRequest tok = postSalesReceipt tok
--
-- -- | Read an sales receipt.
-- readSalesReceiptRequest :: APIEnv
--                    => OAuthTokens
--                    -> SalesReceiptId
--                    -> IO (Either String (QuickBooksResponse SalesReceipt))
-- readSalesReceiptRequest (OAuth1 tok) iId = readSalesReceiptRequestOAuth tok iId
-- readSalesReceiptRequest (OAuth2 tok) iId = readSalesReceiptRequestOAuth2 tok iId
--
-- --- OAuth 1 ---
-- readSalesReceiptRequestOAuth :: APIEnv
--                    => OAuthToken
--                    -> SalesReceiptId
--                    -> IO (Either String (QuickBooksResponse SalesReceipt))
-- readSalesReceiptRequestOAuth tok iId = do
--   let apiConfig = ?apiConfig
--   req  <- oauthSignRequest tok =<< parseUrlThrow (escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}#{unSalesReceiptId iId}|])
--   let oauthHeaders = requestHeaders req
--   let req' = req{method = "GET", requestHeaders = oauthHeaders ++ [(hAccept, "application/json")]}
--   resp <-  httpLbs req' ?manager
--   logAPICall req'
--   return $ eitherDecode $ responseBody resp
--
-- --- OAuth 2 ---
-- readSalesReceiptRequestOAuth2 :: APIEnv
--                    => OAuth2.AccessToken
--                    -> SalesReceiptId
--                    -> IO (Either String (QuickBooksResponse SalesReceipt))
-- readSalesReceiptRequestOAuth2 tok iId = do
--   let apiConfig = ?apiConfig
--   let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{salesReceiptURITemplate apiConfig}#{unSalesReceiptId iId}|]
--   -- Made for logging
--   req' <- parseUrlThrow (escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}#{unSalesReceiptId iId}|])
--   case eitherQueryURI of
--     Left err -> return (Left . show $ err)
--     Right queryURI -> do
--       -- Make the call
--       eitherResponse <- qbAuthGetBS ?manager tok queryURI
--       logAPICall req'
--       case eitherResponse of
--         (Left err) -> return (Left . show $ err)
--         (Right resp) -> do
--           return $ eitherDecode resp
--
--
-- -- | Delete an sales receipt.
-- deleteSalesReceiptRequest :: APIEnv
--                      => OAuthTokens
--                      -> SalesReceiptId
--                      -> SyncToken
--                      -> IO (Either String (QuickBooksResponse DeletedSalesReceipt))
-- deleteSalesReceiptRequest (OAuth1 tok) iId syncToken = deleteSalesReceiptRequestOAuth tok iId syncToken
-- deleteSalesReceiptRequest (OAuth2 tok) iId syncToken = deleteSalesReceiptRequestOAuth2 tok iId syncToken
--
--
-- --- OAuth 1 ---
-- deleteSalesReceiptRequestOAuth :: APIEnv
--                      => OAuthToken
--                      -> SalesReceiptId
--                      -> SyncToken
--                      -> IO (Either String (QuickBooksResponse DeletedSalesReceipt))
-- deleteSalesReceiptRequestOAuth tok iId syncToken = do
--   let apiConfig = ?apiConfig
--   req  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}?operation=delete|]
--   req' <- oauthSignRequest tok req{ method = "POST"
--                                   , requestBody    = RequestBodyLBS $ encode body
--                                   , requestHeaders = [ (hAccept, "application/json")
--                                                      , (hContentType, "application/json")
--                                                      ]
--                                   }
--   resp <-  httpLbs req' ?manager
--   logAPICall req'
--   return $ eitherDecode $ responseBody resp
--   where
--     body = object [ ("Id", String (unSalesReceiptId iId))
--                   , ("SyncToken", String (unSyncToken syncToken))
--                   ]
--
-- --- OAuth 2 ---
-- deleteSalesReceiptRequestOAuth2 :: APIEnv
--                      => OAuth2.AccessToken
--                      -> SalesReceiptId
--                      -> SyncToken
--                      -> IO (Either String (QuickBooksResponse DeletedSalesReceipt))
-- deleteSalesReceiptRequestOAuth2 tok iId syncToken = do
--   let apiConfig = ?apiConfig
--   let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{salesReceiptURITemplate apiConfig}?operation=delete|]
--   -- Made for logging
--   req'  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}?operation=delete|]
--   case eitherQueryURI of
--     Left err -> return (Left . show $ err)
--     Right queryURI -> do
--       -- Make the call
--       eitherResponse <- qbAuthPostBS ?manager tok queryURI body
--       logAPICall req'
--       case eitherResponse of
--         (Left err) -> return (Left . show $ err)
--         (Right resp) -> do
--           return $ eitherDecode resp
--   where
--     body = object [ ("Id", String (unSalesReceiptId iId))
--                   , ("SyncToken", String (unSyncToken syncToken))
--                   ]
--
-- -- | email and sales receipt
-- sendSalesReceiptRequest :: APIEnv
--                    => OAuthTokens
--                    -> SalesReceiptId
--                    -> Email.EmailAddress
--                    -> IO (Either String (QuickBooksResponse SalesReceipt))
-- sendSalesReceiptRequest (OAuth1 tok) iId emailAddr = sendSalesReceiptRequestOAuth tok iId emailAddr
-- sendSalesReceiptRequest (OAuth2 tok) iId emailAddr = sendSalesReceiptRequestOAuth2 tok iId emailAddr
--
-- --- OAuth 1 ---
-- sendSalesReceiptRequestOAuth :: APIEnv
--                    => OAuthToken
--                    -> SalesReceiptId
--                    -> Email.EmailAddress
--                    -> IO (Either String (QuickBooksResponse SalesReceipt))
-- sendSalesReceiptRequestOAuth tok iId emailAddr =  do
--   let apiConfig = ?apiConfig
--   req  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}#{unSalesReceiptId iId}/send?sendTo=#{Email.toByteString emailAddr}|]
--   req' <- oauthSignRequest tok req{ method = "POST"
--                                   , requestHeaders = [ (hAccept, "application/json")
--                                                      ]
--                                   }
--   logAPICall req'
--   resp <-  httpLbs req' ?manager
--   return $ eitherDecode $ responseBody resp

salesReceiptURITemplate :: APIConfig -> String
salesReceiptURITemplate APIConfig{..} = [i|https://#{hostname}/v3/company/#{companyId}/salesreceipt/|]

-- --- OAuth 2 ---
-- sendSalesReceiptRequestOAuth2 :: APIEnv
--                    => OAuth2.AccessToken
--                    -> SalesReceiptId
--                    -> Email.EmailAddress
--                    -> IO (Either String (QuickBooksResponse SalesReceipt))
-- sendSalesReceiptRequestOAuth2 tok iId emailAddr =  do
--   let apiConfig = ?apiConfig
--   let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{salesReceiptURITemplate apiConfig}#{unSalesReceiptId iId}/send?sendTo=#{Email.toByteString emailAddr}|]
--   -- Made for logging
--   req'  <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}#{unSalesReceiptId iId}/send?sendTo=#{Email.toByteString emailAddr}|]
--   case eitherQueryURI of
--     Left err -> return (Left . show $ err)
--     Right queryURI -> do
--       -- Make the call
--       eitherResponse <- qbAuthPostOctetStreamBS ?manager tok queryURI Null
--       logAPICall req'
--       case eitherResponse of
--         (Left err) -> return (Left . show $ err)
--         (Right resp) -> do
--           return $ eitherDecode resp


----- Post SalesReceipt -----
postSalesReceipt :: APIEnv
            => OAuthTokens
            -> SalesReceipt
            -> IO (Either String (QuickBooksResponse SalesReceipt))
postSalesReceipt (OAuth1 tok) salesReceipt = postSalesReceiptOAuth tok salesReceipt
postSalesReceipt (OAuth2 tok) salesReceipt = postSalesReceiptOAuth2 tok salesReceipt

--- OAuth 1 ---
postSalesReceiptOAuth :: APIEnv
            => OAuthToken
            -> SalesReceipt
            -> IO (Either String (QuickBooksResponse SalesReceipt))
postSalesReceiptOAuth tok salesReceipt = do
  let apiConfig = ?apiConfig
  req <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}|]
  req' <- oauthSignRequest tok req{ method         = "POST"
                                  , requestBody    = RequestBodyLBS $ encode salesReceipt
                                  , requestHeaders = [ (hAccept, "application/json")
                                                     , (hContentType, "application/json")
                                                     ]
                                  }
  resp <- httpLbs req' ?manager
  logAPICall req'
  return $ eitherDecode $ responseBody resp

--- OAuth 2 ---
postSalesReceiptOAuth2 :: APIEnv
            => OAuth2.AccessToken
            -> SalesReceipt
            -> IO (Either String (QuickBooksResponse SalesReceipt))
postSalesReceiptOAuth2 tok salesReceipt = do
  let apiConfig = ?apiConfig
  let eitherQueryURI = parseURI strictURIParserOptions . pack $ [i|#{salesReceiptURITemplate apiConfig}|]
  -- Made for logging
  req' <- parseUrlThrow $ escapeURIString isUnescapedInURI [i|#{salesReceiptURITemplate apiConfig}|]
  case eitherQueryURI of
    Left err -> return (Left . show $ err)
    Right queryURI -> do
      -- Make the call
      eitherResponse <- qbAuthPostBS ?manager tok queryURI salesReceipt
      logAPICall req'
      case eitherResponse of
        (Left err) -> return (Left . show $ err)
        (Right resp) -> do
          return $ eitherDecode resp
