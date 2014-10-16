{-# LANGUAGE OverloadedStrings #-}

module Handlers
       (RequestHandler,
        fourOhFourHandler,
        resourceHandler,
        indexHandler,
        resumeHandler,
        echoHandler,
        catHandler,
        contactHandler,
        robotsHandler) where

import Data.ByteString.Lazy.Char8 as BSL8

import qualified Text.Blaze.Html.Renderer.Utf8 as HR

import ResponseRequest
import Aux                 (inferContentDescType)
import Views.StaticViews

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

contactHandler :: RequestHandler
contactHandler _ = return $ Response "HTTP/1.1" 200 HTML $
                   HR.renderHtml $ contactView

resumeHandler :: RequestHandler
resumeHandler _ = return $ Response "HTTP/1.1" 200 HTML $
                HR.renderHtml $ resumeView

catHandler :: RequestHandler
catHandler _ = return $ Response "HTTP/1.1" 200 HTML $
               HR.renderHtml $ catView

robotsHandler :: RequestHandler
robotsHandler = resourceHandler "robots.txt"
