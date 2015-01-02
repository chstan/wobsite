{-# LANGUAGE OverloadedStrings #-}

module Views.StaticViews
       (catView,
        resumeView,
        indexView,
        contactView,
        chessView,
        talksAndPapersView,
        projectIndexView,
        booksView,
        blogIndexView
        ) where

import Data.UUID (UUID)
import Data.Monoid (mempty)

import Text.Blaze.Html5
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Resume
import Templates.Partial (standardPartial, justified,
                          renderProjectSynopsis,
                          renderBlogListing,
                          renderBookRecord,
                          renderTalkRecord)

import Data.Project (ProjectSynopsis)
import Data.BlogEntry (BlogListing)
import Data.Book (BookRecord)
import Data.Talk
import Data.Chess (GameRecord)

talksAndPapersContent :: [TalkRecord] -> Html
talksAndPapersContent talks = do
  H.div ! A.class_ "talks-content" $ do
    H.div ! A.class_ "talks-container" $ do
      H.div ! A.class_ "talks-section-header" $
        p $ "Invited talks"
      mapM_ renderTalkRecord invited_talks
  where
    invited_talks = filter invited talks
    _ = filter (not . invited) talks

booksContent :: [BookRecord] -> Html
booksContent books = do
  H.div ! A.class_ "books-content" $ do
    H.div ! A.class_ "books-header" $ do
      p $ "A record of books I've read lately, to come back to later."
    H.div ! A.class_ "books-container" $ do
      mapM_ renderBookRecord books

blogIndexContent :: [BlogListing] -> Html
blogIndexContent bs = do
  H.div ! A.class_ "projects-content" $ do
    mapM_ renderBlogListing bs

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
      p "To see some of what I've been working on, take a look around or\
        \ head over to my GitHub. Alternatively if you want to get in\
        \ touch, send me an email and I'll get back to you quickly."
      p "Thanks for visiting."


indexView :: Html
indexView = standardPartial indexContent

boastAboutRecord :: Maybe GameRecord -> Html
boastAboutRecord Nothing = mempty
boastAboutRecord (Just r) =
  p $ toHtml $ "The engine's record against visitors is " ++ show r ++ "."

chessContent :: UUID -> Maybe GameRecord -> Html
chessContent uuid mr = do
  H.div ! A.id "content-header" $
    H.div ! A.id "resume-link" $ do
      p "Once you've played a move, please give a moment or two for the engine to reply."
      boastAboutRecord mr
  H.div ! A.id "board" ! A.style "width: 600px" $ mempty
  H.script ! A.src "http://cdnjs.cloudflare.com/ajax/libs/json3/3.3.2/json3.min.js" $ mempty
  H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js" $ mempty
  H.link ! A.href "/resource/css/chessboard-0.3.0.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
  H.script ! A.src "/resource/js/chessboard-0.3.0.min.js" $ mempty
  H.script ! A.src "/resource/js/play_chess.js" $ mempty
  H.script ! A.src "/resource/js/chess.js" $ mempty
  H.script $ toHtml $ "var uuid = \"" ++ (show uuid) ++ "\";"
  --H.script $ "var uuid = \"test\";"

chessView :: UUID -> Maybe GameRecord -> Html
chessView uuid mr = standardPartial $ chessContent uuid mr

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

talksAndPapersView :: [TalkRecord] -> Html
talksAndPapersView ts = standardPartial $ talksAndPapersContent ts

booksView :: [BookRecord] -> Html
booksView bs = standardPartial $ booksContent bs

blogIndexView :: [BlogListing] -> Html
blogIndexView bs = standardPartial $ blogIndexContent bs
