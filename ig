#!/usr/bin/env runhaskell
-- -*- mode: haskell -*-

module Main where

import Control.Applicative ( (<$>) )
import Data.Maybe ( fromJust, catMaybes )
import Network.HTTP ( Request(..), RequestMethod(..), Response(..)
                    , HeaderName(..), findHeader
                    , simpleHTTP )
import Network.URI ( URI, parseURI )
import Text.XML.HaXml ( Document(..), Element(..)
                      , CFilter(..), (/>), tag, txt )
import Text.XML.HaXml.Parse ( xmlParse' )
import Text.XML.HaXml.Posn ( Posn(..) )
import Text.XML.HaXml.Pretty ( content )
import Text.Interpol ( (^-^) )

data Entry = Entry { entryTitle :: String
                   , entryLink :: String }

instance Show Entry where
    show e = "<a href=\"" ^-^ entryLink e ^-^ "\">" ^-^ entryTitle e ^-^ "</a>"

data Feed = Feed { feedTitle :: String
                 , feedEntries :: [Entry] }

instance Show Feed where
    show (Feed { feedTitle = title, feedEntries = es }) =
        unlines (concat [ ["<h3>" ^-^ title ^-^ "</h3>"]
                        , ["<ul>"], map show es, ["</ul>"]])

data FeedDescription =
    FeedDescription { fdTitle       :: String
                    , fdUri         :: URI
                    , fdItemFilter  :: CFilter Posn
                    , fdTitleFilter :: CFilter Posn
                    , fdLinkFilter  :: CFilter Posn
                    }

instance Show FeedDescription where
    show fd = "<FeedDescription for " ^-^ fdTitle fd ^-^ ">"

slashdotFeedDesc :: FeedDescription
slashdotFeedDesc =
    FeedDescription
    { fdTitle       = "Slashdot"
    , fdUri         = fromJust (parseURI "http://rss.slashdot.org/Slashdot/slashdot")
    , fdItemFilter  = tag "item"
    , fdTitleFilter = tag "item" /> tag "title" /> txt
    , fdLinkFilter  = tag "item" /> tag "link" /> txt
    }

hackerNewsFeedDesc :: FeedDescription
hackerNewsFeedDesc =
    FeedDescription
    { fdTitle       = "Hacker News"
    , fdUri         = fromJust (parseURI "http://news.ycombinator.com/rss")
    , fdItemFilter  = tag "channel" /> tag "item"
    , fdTitleFilter = tag "item" /> tag "title" /> txt
    , fdLinkFilter  = tag "item" /> tag "comments" /> txt
    }

getUri :: URI -> IO (Either String String)
getUri uri = do
    resp <- simpleHTTP (Request { rqURI = uri
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
                  Nothing  -> return (Left ("bad redirection: " ^-^ show r))
                  Just url -> case parseURI url of
                                Nothing  -> return (Left ("bad redirection: "
                                                          ^-^ url))
                                Just uri -> getUri uri
            _ ->
                return (Left ("unknown http: " ^-^ show r))

parse :: FeedDescription -> String -> Either String Feed
parse fd text = xmlToFeed <$> (xmlParse' "input" text)
  where
    xmlToFeed = Feed (fdTitle fd) . catMaybes . map mkFeed
              . concatMap (fdItemFilter fd) . getContents . getElements

    getElements (Document _ _ es _) = es

    getContents (Elem _ _ cs) = cs

    contentText = show . content

    mkFeed xml = let title = (fdTitleFilter fd) xml
                     link  = (fdLinkFilter fd) xml
                 in case (title, link) of
                      (t : _, l : _) ->
                          Just (Entry { entryTitle = contentText t
                                      , entryLink = contentText l })
                      _ ->
                          Nothing

getFeed :: FeedDescription -> IO (Either String Feed)
getFeed fd = do
    text <- getUri (fdUri fd)
    return (parse fd =<< text)

main :: IO ()
main = do
    print =<< getFeed slashdotFeedDesc
    print =<< getFeed hackerNewsFeedDesc
