{-# LANGUAGE OverloadedStrings #-}

module Templates.Partial
       (navPartial,
        standardPartial,
        renderProjectSynopsis,
        renderBookRecord,
        justified,
        justifiedLeader) where

import Data.Monoid                    (mempty)
import Data.Text

import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Project as Pr
import Data.Book as Bk

bookReadDate :: Bool -> Text -> Html
bookReadDate False _ = p $ preEscapedString "&mdash;"
bookReadDate _ date = p $ toHtml date

renderBookRecord :: BookRecord -> Html
renderBookRecord
  BookRecord {Bk.title=b_title, Bk.author=b_author,
              Bk.completion_date=b_date,
              Bk.impression=b_imp, Bk.finished=b_finished} = do
    H.div $ do
      cite $ toHtml b_title
      justified (p $ toHtml b_author)
                (bookReadDate b_finished b_date)
    H.div ! A.class_ "book-sep" $ mempty

projectLink :: Text -> Text -> Html
projectLink "#" t =
  h2 $ toHtml t
projectLink l t =
  a ! A.href (H.toValue (Data.Text.concat ["/projects/", l]))
    $ h2 $ toHtml t


projectImageStyle :: Text -> Text
projectImageStyle img_url =
  Data.Text.concat ["background: url(/resource/", img_url,
                    ") no-repeat center 0;",
                    "background-size: cover"]

renderProjectSynopsis :: ProjectSynopsis -> Html
renderProjectSynopsis
  ProjectSynopsis {Pr.title=t, Pr.label=l,
                   picture_file=pf, description=d} =
    H.div ! A.class_ "project-container" $ do
      H.div ! A.class_ "project-image" !
        A.style (H.toValue $ projectImageStyle pf) $ mempty
      H.div ! A.class_ "project-content" $ do
        projectLink l t
        p ! A.class_ "project-description" $ toHtml d


navPartial :: Html
navPartial =
  nav $ do
    h3 ! (A.id "name-header") $ "conrad stansbury"
    ul $ do
      li $ H.a ! A.href "./" $ "/"
      li $ H.a ! A.href "/resume" $ "resume"
      li $ H.a ! A.href "/projects" $ "projects"
      li $ H.a ! A.href "http://github.com/chstan" $ "github"
      li $ H.a ! A.href "/contact" $ "contact"
      li $ H.a ! A.href "#" $ "writing"
      li $ H.a ! A.href "/books" $ "reading"

standardPartial :: Html -> Html
standardPartial content = docTypeHtml $ do
  H.head $ do
    H.title "Conrad Stansbury"
    link ! A.href "http://fonts.googleapis.com/css?family=Roboto+Slab"
         ! A.rel "stylesheet" ! A.type_ "text/css"
    link ! A.href "http://fonts.googleapis.com/css?family=Roboto:400,300"
         ! A.rel "stylesheet" ! A.type_ "test/css"
    link ! A.href "/resource/style_test.css"
         ! A.rel "stylesheet" ! A.type_ "text/css"
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.div ! A.class_ "container2" $ do
        navPartial
      H.div ! A.class_ "landing-content" $ do
        content

justified :: Html -> Html -> Html
justified l r = do
  H.div ! A.class_ "just-container" $ do
    H.div ! A.class_ "just-left-container" $ l
    H.div ! A.class_ "just-right-container" $ r

justifiedLeader :: Html -> Html -> Html
justifiedLeader l r = do
  H.div ! A.class_ "just-container content-leader" $ do
    H.div ! A.class_ "just-left-container" $ l
    H.div ! A.class_ "just-right-container" $ r
