{-# LANGUAGE OverloadedStrings #-}

module Templates.Resume
       (resumeBuilder,
        resumeInserts) where

import Data.Monoid (mempty)
import Control.Monad (forM_)

import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Partial (justified, justifiedLeader)

resumeSection :: Html -> Html -> Html
resumeSection t c = do
  H.div ! A.class_ "resume-section-title" $ t
  H.div ! A.class_ "resume-section-content" $ c

resumeParagraphs :: String -> Html
resumeParagraphs ps = H.div ! A.class_ "resume-paragraphs" $
                      forM_ (Prelude.lines ps) (H.p . toHtml)

resumeInserts :: [Html]
resumeInserts =
  [ resumeSection (h4 $ "Objective") $ (p
    "To obtain a summer research position or internship\
    \ in software engineering or data science."),

    resumeSection (h4 $ "Education") $ do
      justified (p $ i "Stanford University") (p "GPA 3.98/4.00")
      H.div $ p "Pursuing BS with Honors in Physics, theory concentration"
      H.div $ p "Expected June 2015",

    resumeSection (h4 $ "Computer Skills") $ do
      justified (p $ i "Proficient") (p "C{++}, Python, Clojure, UNIX, LaTeX")
      justified (p $ i "Experienced") (p "Ruby, Haskell, HTML/CSS, JavaScript, \
                                         \Mathematica/Matlab"),

    resumeSection (h4 $ "Coursework") $ (p
      "Computer Organization and Systems, Algorithms, Optimization and Graduate \
      \Algorithms, Data Mining, Data Mining for Cyber Security, Linear Dynamical \
      \Systems, Convex Optimization (Series)"),

    resumeSection (h4 "Experience") $ do
      justifiedLeader (p $ do {b "Research Intern"; " SLAC National Laboratory" })
                      (p $ preEscapedString "June 2013 &ndash;")
      resumeParagraphs "Working with the ATLAS group at SLAC on two data \
                       \analysis projects to improve resolution of experiments \
                       \conducted at the LHC. From June 2013 to June 2014, \
                       \worked on investigating the applicability of machine \
                       \learning techniques to jet finding and developed a \
                       \tagging scheme competitive with other state-of-the-art \
                       \techniques.\nMost recently, my research has been into \
                       \using mixture modeling as a jet finding techinque. Aside \
                       \from the physics, this has involved writing around 10k \
                       \lines of algorithmic C++ to conduct and automate the \
                       \analyses."
      justifiedLeader (p $ do {b "Data Analyst Intern"; " Mobile Posse" })
                      (p $ preEscapedString "June 2011 &ndash; September 2011")
      resumeParagraphs "Performed data analysis for a small startup to help \
                       \them better understand customer turnover. Also proposed \
                       \and designed an automated analysis and alert framework \
                       \which would allow monitoring of their hundreds of \
                       \different device platforms.",

    resumeSection (h4 $ "Projects") $ do
      justifiedLeader (p $ b $ "Chess Engine") mempty
      resumeParagraphs  "Designed and built a chess engine in C as a way of \
                        \exploring my hobby for chess and understanding how \
                        \systems level languages can be leveraged to build \
                        \complex applications."
      justifiedLeader (p $ b $ "Web Server") mempty
      resumeParagraphs  "Currently writing a web server from the ground up \
                        \using Haskell + HTML5/CSS + JS to learn about web \
                        \technologies in a holistic way and to host a website \
                        \for completed and ongoing projects, research, \
                        \contact information, public documents, and papers. \
                        \You're looking at it! For the source please visit \
                        \/projects or my GitHub.",
    resumeSection (do
      h4 $ "Activities +"
      h4 $ "Interests")
        (p $ "Hiking, running, writing short stories, cooking, category theory")
  ]

resumeBuilder :: [Html] -> Html
resumeBuilder inserts =
  article ! A.id "resume" $ do
    H.div ! A.class_ "hruled" ! A.id "resume-header" $ do
      h3 "Conrad Stansbury"
      forM_ ["531 Lasuen Mall", "PO Box 17562", "Stanford, CA 94305"] H.p
    forM_ inserts (H.div ! A.class_ "resume-section")
