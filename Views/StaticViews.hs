{-# LANGUAGE OverloadedStrings #-}

module Views.StaticViews
       (catView,
        resumeView,
        indexView,
        contactView,
        projectIndexView,
        booksView
        ) where

import Control.Monad (mapM_)

import Text.Blaze.Html5
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Resume
import Templates.Partial (standardPartial, justified,
                          renderProjectSynopsis,
                          renderBookRecord)

import Data.Project
import Data.Book (BookRecord)

booksContent :: [BookRecord] -> Html
booksContent books = do
  H.div ! A.class_ "books-content" $ do
    H.div ! A.class_ "books-header" $ do
      p $ "A record of books I've read lately, to come back to later."
    H.div ! A.class_ "books-container" $ do
      mapM_ renderBookRecord books

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
indexContent =
  H.div ! A.id "content-header" $ do
    H.div ! A.id "statement" $ do
      p "Hi, I'm Conrad."
      p "I currently attend Stanford University where I major in physics."
      p "To see some of what I've been work on, take a look around or\
        \ head over to my GitHub. Alternatively if you want to get in\
        \ touch, send me an email and I'll get back to you quickly."
      p "Thanks for visiting."


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

booksView :: [BookRecord] -> Html
booksView bs = standardPartial $ booksContent bs
