{-# LANGUAGE OverloadedStrings #-}

module Views.StaticViews
       (catView,
        resumeView,
        indexView
        ) where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Resume
import Templates.Partial (standardPartial)

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
