#!/usr/bin/env runhaskell
-- -*- mode: haskell -*-

module Main where

import Control.Applicative ( (<$>) )
import Data.Maybe ( fromJust, catMaybes )
import Network.HTTP ( Request(..), RequestMethod(..), Response(..)
                    , HeaderName(..), findHeader
                    , simpleHTTP )
import Network.URI ( parseURI )
import Text.XML.HaXml ( Document(..), Element(..)
                      , (/>), tag, txt )
import Text.XML.HaXml.Parse ( xmlParse' )
import Text.XML.HaXml.Pretty ( content )
import Text.Interpol ( (^-^) )

data Entry = Entry { entryTitle :: String
                   , entryLink :: String }

instance Show Entry where
    show e = "<a href=\"" ^-^ entryLink e ^-^ "\">" ^-^ entryTitle e ^-^ "</a>"

data Feed = Feed { feedEntries :: [Entry] }

instance Show Feed where
    show (Feed { feedEntries = es }) =
        unlines (concat [["<ul>"], map show es, ["</ul>"]])

getUrl :: String -> IO (Either String String)
getUrl url = do
    resp <- simpleHTTP (Request { rqURI = fromJust (parseURI url)
                                , rqMethod = GET, rqHeaders = []
                                , rqBody = "" })
    case resp of
      Left err ->
          return (Left ("http error: " ^-^ err))
      Right r ->
          case rspCode r of
            (2, _, _) ->
                return (Right (rspBody r))
            (3, _, _) ->
                case findHeader HdrLocation r of
                  Nothing   -> return (Left ("bad redirection: " ^-^ show r))
                  Just url' -> getUrl url'
            _ ->
                return (Left ("unknown http: " ^-^ show r))

parse :: String -> Either String Feed
parse text = xmlToFeed <$> (xmlParse' "input" text)
  where
    xmlToFeed = Feed . catMaybes . map mkFeed
              . concatMap (tag "channel" /> tag "item") . getContents . getElements

    getElements (Document _ _ es _) = es

    getContents (Elem _ _ cs) = cs

    contentText = show . content

    mkFeed xml = let title = (tag "item" /> tag "title" /> txt) xml
                     link  = (tag "item" /> tag "comments" /> txt) xml
                 in case (title, link) of
                      (t : _, l : _) ->
                          Just (Entry { entryTitle = contentText t
                                      , entryLink = contentText l })
                      _ ->
                          Nothing

main :: IO ()
main = do
--    text <- getUrl "http://rss.slashdot.org/Slashdot/slashdot"
    text <- getUrl "http://news.ycombinator.com/rss"
    print (parse =<< text)
