{-# LANGUAGE OverloadedStrings #-}
module Cook.Downloads where

import Cook.Types
import Cook.Util

import Data.List (foldl')
import Control.Lens ((^.), (^?))
import Network.Wreq
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getUrlHash :: DownloadUrl -> IO SHA1
getUrlHash (DownloadUrl url) =
    do r <- head_ (T.unpack url)
       let mLocation = r ^? responseHeader "Location"
       case mLocation of
         Just redir ->
             fail ("The download url " ++ T.unpack url ++ " redirected my to "
                   ++ BSC.unpack redir ++ ". That's not supported yet.")
         Nothing -> return ()
       case foldl' (handleDepTag r) [] depTags of
         [] ->
             do let allRespH = r ^. responseHeaders
                fail ("The download url " ++ T.unpack url
                   ++ " didn't set any identifying headers (Last-Modified, ETag, Content-MD5). "
                   ++ "Found headers: " ++ show allRespH
                     )
         xs -> return $ quickHash (T.encodeUtf8 url : xs)
    where
      handleDepTag r xs depTag =
          case r ^? responseHeader depTag of
            Just tagValue ->
                tagValue:xs
            Nothing -> xs
      depTags =
          [ "Last-Modified"
          , "ETag"
          , "Content-MD5"
          ]
