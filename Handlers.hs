{-# LANGUAGE OverloadedStrings #-}

module Handlers
       (RequestHandler,
        projectIndexHandler,
        fourOhFourHandler,
        resourceHandler,
        indexHandler,
        resumeHandler,
        echoHandler,
        catHandler,
        contactHandler,
        booksHandler,
        blogIndexHandler,
        blogEntryHandler,
        staticPageHandler,
        robotsHandler) where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Lazy.Encoding
import Data.ByteString.Lazy.Char8 as BSL8
import Data.Aeson          (eitherDecode)
import Control.Applicative ((<$>))

import Data.TCache.Memoization (cachedByKey)

import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import ResponseRequest
import Aux                 (inferContentDescType)
import Views.StaticViews
import Views.BlogEntry

import Data.BlogEntry (lookupBlogEntry, markdown_location)

type RequestHandler = Request -> IO Response

cachedReadFile :: String -> IO ByteString
cachedReadFile s = cachedByKey s 0 $ BSL8.readFile s

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 UNZIP PLAIN (pack $ show req)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404 UNZIP
                      PLAIN (pack $ "Oh man! 404...")

fileHandler :: String -> RequestHandler
fileHandler s _ = do
  contents <- cachedReadFile s
  return $ Response "HTTP/1.1" 200 UNZIP (inferContentDescType s) contents

resourceHandler :: String -> RequestHandler
resourceHandler s = fileHandler ("res/" ++ s)

indexHandler :: RequestHandler
indexHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
                 HR.renderHtml $ indexView

contactHandler :: RequestHandler
contactHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
                   HR.renderHtml $ contactView

resumeHandler :: RequestHandler
resumeHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
                HR.renderHtml $ resumeView

catHandler :: RequestHandler
catHandler _ = return $ Response "HTTP/1.1" 200 UNZIP HTML $
               HR.renderHtml $ catView

robotsHandler :: RequestHandler
robotsHandler = resourceHandler "robots.txt"

projectIndexHandler :: RequestHandler
projectIndexHandler req = do
  dec <- eitherDecode <$> cachedReadFile "res/project_descriptions.json"
  case dec of
   Left _ -> fourOhFourHandler req -- Meh, could be a better response.
   Right projects -> return $ Response "HTTP/1.1" 200 UNZIP HTML $
                     HR.renderHtml $ projectIndexView projects

booksHandler :: RequestHandler
booksHandler req = do
  dec <- eitherDecode <$> cachedReadFile "res/books.json"
  case dec of
   Left _ -> fourOhFourHandler req
   Right books -> return $ Response "HTTP/1.1" 200 UNZIP HTML $
                  HR.renderHtml $ booksView books

staticPageHandler :: String -> RequestHandler
staticPageHandler entryName req = do
  dec <- eitherDecode <$> cachedReadFile "res/pages.json"
  case (lookupBlogEntry entryName dec) of
    Nothing -> fourOhFourHandler req
    Just listing -> do
      content <- fmap decodeUtf8 $ cachedReadFile ("res/" ++ (T.unpack m_location))
      return $ Response "HTTP/1.1" 200 UNZIP HTML $
        HR.renderHtml $ blogEntryView listing content
       where m_location = markdown_location listing


blogIndexHandler :: RequestHandler
blogIndexHandler req = do
  dec <- eitherDecode <$> cachedReadFile "res/blog_entries.json"
  case dec of
   Left _ -> fourOhFourHandler req
   Right entries -> return $ Response "HTTP/1.1" 200 UNZIP HTML $
                    HR.renderHtml $ blogIndexView entries

blogEntryHandler :: String -> RequestHandler
blogEntryHandler entryName req = do
  dec <- eitherDecode <$> cachedReadFile "res/blog_entries.json"
  case (lookupBlogEntry entryName dec) of
    Nothing -> fourOhFourHandler req
    Just listing -> do
      content <- fmap decodeUtf8 $ cachedReadFile ("res/" ++ (T.unpack m_location))
      return $ Response "HTTP/1.1" 200 UNZIP HTML $
        HR.renderHtml $ blogEntryView listing content
       where m_location = markdown_location listing
