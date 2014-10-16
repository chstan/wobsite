{-# LANGUAGE OverloadedStrings #-}

module Handlers
       (RequestHandler,
        fourOhFourHandler,
        resourceHandler,
        indexHandler,
        resumeHandler,
        echoHandler,
        catHandler) where

import Data.ByteString.Lazy.Char8 as BSL8

import Data.Monoid (mempty)
import Control.Monad (forM_)

import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import ResponseRequest
import Aux                 (inferContentDescType)
import Views.StaticViews   (indexView, resumeView, catView)

type RequestHandler = Request -> IO Response

echoHandler :: RequestHandler
echoHandler req = return $ Response "HTTP/1.1" 200 PLAIN (pack $ show req)

fourOhFourHandler :: RequestHandler
fourOhFourHandler _ = return $ Response "HTTP/1.1" 404
                      PLAIN (pack $ "Oh man! 404...")

fileHandler :: String -> RequestHandler
fileHandler s _ = do
  contents <- BSL8.readFile s
  return $ Response "HTTP/1.1" 200 (inferContentDescType s) contents

resourceHandler :: String -> RequestHandler
resourceHandler s = fileHandler ("res/" ++ s)

indexHandler :: RequestHandler
indexHandler _ = return $ Response "HTTP/1.1" 200 HTML $
                 HR.renderHtml $ indexView

resumeHandler :: RequestHandler
resumeHandler _ = return $ Response "HTTP/1.1" 200 HTML $
                HR.renderHtml $ resumeView

catHandler :: RequestHandler
catHandler _ = return $ Response "HTTP/1.1" 200 HTML $
               HR.renderHtml $ catView
