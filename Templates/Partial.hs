{-# LANGUAGE OverloadedStrings #-}

module Templates.Partial
       (navPartial,
        standardPartial,
        justified,
        justifiedLeader) where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
      li $ H.a ! A.href "#" $ "->"

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
