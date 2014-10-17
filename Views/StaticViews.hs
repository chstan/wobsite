{-# LANGUAGE OverloadedStrings #-}

module Views.StaticViews
       (catView,
        resumeView,
        indexView,
        contactView,
        projectIndexView
        ) where

import Control.Monad (mapM_)

import Text.Blaze.Html5
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Resume
import Templates.Partial (standardPartial, justified,
                          renderProjectSynopsis)

import Data.Project

projectIndexContent :: [ProjectSynopsis] -> Html
projectIndexContent ps = do
  H.div ! A.class_ "projects-content" $ do
    mapM_ renderProjectSynopsis ps

contactContent :: Html
contactContent = do
  H.div ! A.class_ "content-container contact-container" $ do
    justified (p "Email")
              (p "chstan at {slac.}stanford.edu")
    justified (p "Phone") (p $ preEscapedString
                           "703 &nbsp;317 &nbsp;7012")

indexContent :: Html
indexContent = do
  h3 "This is a heading."
  p "This is a paragraph."

indexView :: Html
indexView = standardPartial indexContent

catView :: Html
catView = docTypeHtml $ do
  H.head $ link ! A.href "/resource/style_test.css" ! A.rel "stylesheet"
  H.body $ img ! A.src  "/resource/kash.jpg"

resumeContent :: Html
resumeContent = do
  H.div ! A.id "content-header" $
    H.div ! A.id "resume-link" $
      p $ do
        "A pdf copy of the resume is also available for download "
        a ! A.href "resource/resume.pdf" $ "here"
        "."
  resumeBuilder resumeInserts

resumeView :: Html
resumeView = standardPartial resumeContent

contactView :: Html
contactView = standardPartial contactContent

projectIndexView :: [ProjectSynopsis] -> Html
projectIndexView ps = standardPartial $ projectIndexContent ps
