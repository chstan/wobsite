{-# LANGUAGE OverloadedStrings #-}

module Views.BlogEntry
       (blogEntryView
       ) where

import Data.Monoid (mempty)
import Data.Text.Internal.Lazy
import Text.Markdown
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Partial (standardPartial)
import Data.BlogEntry

blogEntryView :: BlogListing -> Text -> Html
blogEntryView _ content = standardPartial $ do
  link ! A.rel "stylesheet" ! A.href "//cdnjs.cloudflare.com/ajax/libs/\
                                     \highlight.js/8.3/styles/github.min.css"
  H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/\
                   \highlight.js/8.3/highlight.min.js" $ mempty
  H.script $ "hljs.initHighlightingOnLoad();"
  H.div ! A.class_ "projects-content" $ do
    markdown def content
