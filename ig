#!/usr/bin/env runhaskell
-- -*- mode: haskell -*-

module Main where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM_, when )
import Data.Maybe ( fromJust, catMaybes )
import Data.String ( fromString )
import Network.HTTP ( Request(..), RequestMethod(..), Response(..)
                    , HeaderName(..), findHeader
                    , simpleHTTP )
import Network.URI ( URI, parseURI )
import System.IO ( hPutStrLn, stderr )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty ( renderHtml )
import Text.Html ( HTML(..) )
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

defaultFeedDesc :: FeedDescription
defaultFeedDesc =
    FeedDescription
    { fdTitle       = "The Mysterious Feed"
    , fdUri         = fromJust (parseURI "http://boom.org/kaboom.rss")
    , fdItemFilter  = tag "channel" /> tag "item"
    , fdTitleFilter = tag "item" /> tag "title" /> txt
    , fdLinkFilter  = tag "item" /> tag "link" /> txt
    }

slashdotFeedDesc :: FeedDescription
slashdotFeedDesc =
    defaultFeedDesc
    { fdTitle      = "Slashdot"
    , fdUri        = fromJust (parseURI "http://rss.slashdot.org/Slashdot/slashdot")
    , fdItemFilter = tag "item"
    }

hackerNewsFeedDesc :: FeedDescription
hackerNewsFeedDesc =
    defaultFeedDesc
    { fdTitle      = "Hacker News"
    , fdUri        = fromJust (parseURI "http://news.ycombinator.com/rss")
    , fdLinkFilter = tag "item" /> tag "comments" /> txt
    }

techdirtFeedDesc :: FeedDescription
techdirtFeedDesc =
    defaultFeedDesc
    { fdTitle = "Techdirt"
    , fdUri   = fromJust (parseURI "http://www.techdirt.com/techdirt_rss.xml")
    }

osNewsFeedDesc :: FeedDescription
osNewsFeedDesc =
    defaultFeedDesc
    { fdTitle       = "OSNews"
    , fdUri         = fromJust (parseURI "http://www.osnews.com/files/recent.xml")
    }

wiredFeedDesc :: FeedDescription
wiredFeedDesc =
    defaultFeedDesc
    { fdTitle       = "Wired Top Stories"
    , fdUri         = fromJust (parseURI "http://feeds.wired.com/wired/index")
    }

bookmarksFeedDesc :: FeedDescription
bookmarksFeedDesc =
    defaultFeedDesc
    { fdTitle       = "Bookmarks"
    , fdUri         = fromJust (parseURI "http://www.abstractbinary.org/bookmarks.rss")
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

page :: [Feed] -> H.Html
page fs = H.docTypeHtml $ do
    H.head $ do
        H.title (fromString "Home")
    H.body $ forM_ fs feedToHtml
  where
    entryToHtml e = do
        H.a H.! A.href (fromString (entryLink e)) $ do
           fromString (entryTitle e)

    feedToHtml feed = do
        H.div $ do
            H.h3 (fromString (feedTitle feed))
            H.ul $ forM_ (feedEntries feed) (H.li . entryToHtml)

getFeeds :: [FeedDescription] -> IO ([String], [Feed])
getFeeds [] = return ([], [])
getFeeds (fd : fds) = do
    feedOrError <- getFeed fd
    (errs, feeds) <- getFeeds fds
    case feedOrError of
      Left err   -> return (err : errs, feeds)
      Right feed -> return (errs, feed : feeds)

main :: IO ()
main = do
    (errs, feeds) <- getFeeds [ slashdotFeedDesc
                              , hackerNewsFeedDesc
                              , techdirtFeedDesc
                              , osNewsFeedDesc
                              , wiredFeedDesc
                              ]
    when (not (null errs))
         (hPutStrLn stderr (unlines errs))
    putStrLn (renderHtml (page feeds))
