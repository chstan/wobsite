{-# LANGUAGE OverloadedStrings #-}

module Templates.Partial
       (navPartial,
        standardPartial,
        renderProjectSynopsis,
        renderBlogListing,
        renderBookRecord,
        renderTalkRecord,
        justified,
        googleAnalyticsHook,
        justifiedLeader) where

import Data.Monoid                    (mempty)
import Data.Text

import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Project as Pr
import Data.Book as Bk
import Data.Talk as Tk
import Data.BlogEntry as Bg

bookReadDate :: Bool -> Text -> Html
bookReadDate False _ = p $ preEscapedString "&mdash;"
bookReadDate _ d = p $ toHtml d

talkLink :: Text -> Html
talkLink "#" =
  a $ "[upcoming]"
talkLink l
  | Data.Text.head l == '/' = a ! A.href (H.toValue l) $ "[slides]"
  | otherwise = a ! A.href (H.toValue (Data.Text.concat ["/resource/", l]))
                  $ "[slides]"


renderTalkRecord :: TalkRecord -> Html
renderTalkRecord
  (TalkRecord tName tDate tLocation tPresentationTitle tPresentationURL _) = do
    H.div ! A.class_ "talks-wrapper" $ do
      p $ do
        strong $ toHtml (" " ++ (unpack tPresentationTitle))
        toHtml $ " at " ++ (unpack tName) ++ ", " ++ (unpack tLocation) ++
                 ", " ++ (unpack tDate) ++ ".  "
        talkLink tPresentationURL

renderBookRecord :: BookRecord -> Html
renderBookRecord
  (BookRecord b_title b_author b_date _ b_finished) = do
    H.div $ do
      cite $ toHtml b_title
      justified (p $ toHtml b_author)
                (bookReadDate b_finished b_date)
    H.div ! A.class_ "book-sep" $ mempty

blogLink :: Text -> Text -> Html
blogLink "#" t =
  h2 $ toHtml t
blogLink l t =
  a ! A.href (H.toValue (Data.Text.concat ["/writing/", l]))
    $ h2 $ toHtml t

renderBlogListing :: BlogListing -> Html
renderBlogListing
  BlogListing {Bg.title = t, Bg.label = l,
               Bg.picture_file = pf, Bg.description = d} =
    H.div ! A.class_ "project-container" $ do
      H.div ! A.class_ "project-content" $ do
        blogLink l t
        p ! A.class_ "project-description" $ toHtml d

projectLink :: Text -> Text -> Html
projectLink "#" t =
  h2 $ toHtml t
projectLink l t
  | Data.Text.head l == '/' = a ! A.href (H.toValue l)
                              $ h2 $ toHtml t
  | otherwise = a ! A.href (H.toValue (Data.Text.concat ["/static/", l]))
                $ h2 $ toHtml t

projectImageStyle :: Text -> Text
projectImageStyle img_url =
  Data.Text.concat ["background: url(/resource/", img_url,
                    ") no-repeat center 0;",
                    "background-size: cover"]

renderProjectSynopsis :: ProjectSynopsis -> Html
renderProjectSynopsis
  ProjectSynopsis {Pr.title=t, Pr.label=l,
                   Pr.picture_file=pf, Pr.description=d} =
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
      li $ H.a ! A.href "/conferences" $ "talks"
      li $ H.a ! A.href "http://github.com/chstan" $ "github"
      li $ H.a ! A.href "/contact" $ "contact"
      li $ H.a ! A.href "/writing" $ "writing"
      li $ H.a ! A.href "/books" $ "reading"
      li $ H.a ! A.href "/scheme" $ "scheme"
      li $ H.a ! A.href "/chess" $ "chess"


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
    googleAnalyticsHook
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

googleAnalyticsHook :: Html
googleAnalyticsHook =
  H.script $
  "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){\
  \(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),\
  \m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)\
  \})(window,document,'script','//www.google-analytics.com/analytics.js','ga');\
  \ga('create', 'UA-55955707-1', 'auto');\
  \ga('send', 'pageview');"
