#!/usr/bin/env runhaskell
-- -*- mode: haskell -*-
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Exception ( Exception(..), SomeException )
import Control.Monad ( forM_, when )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe ( fromJust, catMaybes )
import Data.Monoid ( mempty )
import Data.String ( fromString )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import Network.HTTP ( Request(..), RequestMethod(..), Response(..)
                    , HeaderName(..), findHeader
                    , simpleHTTP )
import Network.URI ( URI, parseURI )
import System.IO ( hPutStrLn, stderr )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import Text.Html ( HTML(..) )
import Text.XML
import Text.XML.Cursor
import Text.Interpol ( (^-^) )

data Entry = Entry { entryTitle :: Text
                   , entryLink :: Text }

instance Show Entry where
    show e = "<a href=\"" ^-^ entryLink e ^-^ "\">" ^-^ entryTitle e ^-^ "</a>"

data Feed = Feed { feedTitle   :: String
                 , feedEntries :: [Entry] }

instance Show Feed where
    show (Feed { feedTitle = title, feedEntries = es }) =
        unlines (concat [ ["<h3>" ^-^ title ^-^ "</h3>"]
                        , ["<ul>"], map show es, ["</ul>"]])

data FeedDescription =
    FeedDescription { fdTitle       :: String
                    , fdUri         :: URI
                    , fdItemFilter  :: Cursor -> [Cursor]
                    , fdTitleFilter :: Cursor -> [Text]
                    , fdLinkFilter  :: Cursor -> [Text]
                    }

instance Show FeedDescription where
    show fd = "<FeedDescription for " ^-^ fdTitle fd ^-^ ">"

newtype HttpException = HttpException String
    deriving ( Typeable, Show )

instance Exception HttpException

defaultFeedDesc :: FeedDescription
defaultFeedDesc =
    FeedDescription
    { fdTitle       = "The Mysterious Feed"
    , fdUri         = fromJust (parseURI "http://boom.org/kaboom.rss")
    , fdItemFilter  = \cursor -> cursor $// laxElement (fromString "item")
    , fdTitleFilter = \cursor -> cursor $// laxElement (fromString "title")
                                        &// content
    , fdLinkFilter  = \cursor -> cursor $// laxElement (fromString "link")
                                        &// content
    }

slashdotFeedDesc :: FeedDescription
slashdotFeedDesc =
    defaultFeedDesc
    { fdTitle      = "Slashdot"
    , fdUri        = fromJust (parseURI "http://rss.slashdot.org/Slashdot/slashdot")
    }

hackerNewsFeedDesc :: FeedDescription
hackerNewsFeedDesc =
    defaultFeedDesc
    { fdTitle      = "Hacker News"
    , fdUri        = fromJust (parseURI "http://news.ycombinator.com/rss")
    , fdLinkFilter = \cursor -> cursor $// laxElement (fromString "comments")
                                       &// content
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

getUri :: URI -> IO (Either SomeException BS.ByteString)
getUri uri = do
    resp <- simpleHTTP (Request { rqURI     = uri
                                , rqMethod  = GET
                                , rqHeaders = []
                                , rqBody    = fromString "" })
    case resp of
      Left err ->
          httpException ("http error: " ^-^ err)
      Right r ->
          case rspCode r of
            (2, _, _) ->
                return (Right (rspBody r))
            (3, _, _) ->
                case findHeader HdrLocation r of
                  Nothing  -> httpException ("bad redirection: " ^-^ show r)
                  Just url -> case parseURI url of
                                Nothing  -> httpException ("bad redirection: "
                                                           ^-^ url)
                                Just uri -> getUri uri
            _ ->
                httpException ("unknown http: " ^-^ show r)
  where
    httpException = return . Left . toException . HttpException

parse :: FeedDescription -> BS.ByteString -> Either SomeException Feed
parse fd text = xmlToFeed <$> parseLBS def text
  where
    xmlToFeed doc =
        let cursor = fromDocument doc in
        let items = fdItemFilter fd cursor in
        Feed (fdTitle fd) . catMaybes $ map mkFeed items

    mkFeed cursor =
        let title = fdTitleFilter fd cursor
            link  = fdLinkFilter fd cursor
        in case (title, link) of
             (t : _, l : _) ->
                 Just (Entry { entryTitle = t
                             , entryLink = l })
             _ ->
                 Nothing

getFeed :: FeedDescription -> IO (Either SomeException Feed)
getFeed fd = do
    text <- getUri (fdUri fd)
    return (parse fd =<< text)

page :: [Feed] -> H.Html
page fs = H.docTypeHtml $ do
    H.head $ do
        H.meta
            H.! A.httpEquiv (fromString "Content-Type")
            H.! A.content (fromString "text/html;charset=utf-8")
        H.script
            H.! A.type_ (fromString "text/javascript")
            H.! A.src (fromString "js/ig.js")
            $ mempty
        H.link
            H.! A.rel (fromString "styleSheet")
            H.! A.type_ (fromString "text/css")
            H.! A.href (fromString "css/ig.css")
        H.title (fromString "Home")
    H.body $ forM_ fs feedToHtml
  where
    entryToHtml e = do
        H.a H.! A.href (H.toValue (entryLink e)) $ do
           H.toMarkup (entryTitle e)

    feedToHtml feed = do
        H.div H.! A.class_ (fromString "feed") $ do
            H.h3 (fromString (feedTitle feed))
            H.ul $ forM_ (feedEntries feed) (H.li . entryToHtml)

getFeeds :: [FeedDescription] -> IO ([SomeException], [Feed])
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
         (hPutStrLn stderr (unlines (map show errs)))
    BS.putStrLn (renderHtml (page feeds))
