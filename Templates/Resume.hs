{-# LANGUAGE OverloadedStrings #-}

module Templates.Resume
       (resumeBuilder,
        resumeInserts) where

import Data.Monoid (mempty)
import Data.List (intercalate)
import Control.Monad (forM_)

import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Talk

import Templates.Partial (justified, justifiedLeader, renderTalkRecord)

resumeSection :: Html -> Html -> Html
resumeSection t c = do
  H.div ! A.class_ "resume-section-title" $ t
  H.div ! A.class_ "resume-section-content" $ c

resumeParagraphs :: String -> Html
resumeParagraphs ps = H.div ! A.class_ "resume-paragraphs" $
                      forM_ (Prelude.lines ps) (H.p . toHtml)

relevantCoursesPhysics :: [String]
relevantCoursesPhysics =
  [ "Quantum Field Theory I",
    "General Relativity",
    "Low Temperature Physics Lab",
    "Lasers Laboratory",
    "Statistical Mechanics and Thermodynamics I + II",
    "Hamiltonian Mechanics",
    "Electricity and Magnetism",
    "Quantum Mechanics",
    "Introduction to Particle Physics",
    "Introduction to Cosmology",
    "Electrons and Photons" ]

relevantCoursesMath :: [String]
relevantCoursesMath =
  [ "Graduate Algebra I + II",
    "Functional Analysis",
    "Convex Optimization I + II" ]

relevantCoursesCS :: [String]
relevantCoursesCS =
  [ "Computer Organization and Systems",
    "Algorithms",
    "Optimization and Graduate Algorithms",
    "Data Mining",
    "Data Mining for Cyber Security",
    "Linear Dynamical Systems" ]

resumeInserts :: [TalkRecord] -> [Html]
resumeInserts ts =
  [ --resumeSection (h4 $ "Objective") $
    --p "To obtain a programming internship at Ginkgo Bioworks.",

    resumeSection (h4 $ "Education") $ do
      justified (p $ i "Stanford University") (p "GPA 4.01")
      H.div $ p "Graduated June 2015, BS Physics with Distinction and Honors,"
      H.div $ p "concentration in theoretical physics",

    resumeSection (h4 $ "Coursework") $ do
      --p $ toHtml $ (intercalate ", " relevantCoursesPhysics)
      p $ toHtml $ (intercalate ", " relevantCoursesCS)
      p $ toHtml $ (intercalate ", " relevantCoursesMath),

    resumeSection (h4 "Experience") $ do
      justifiedLeader (p $ do {b "Research Intern"; " SLAC National Laboratory" })
                      (p $ preEscapedString "June 2013 &ndash; present")
      resumeParagraphs "Working with the ATLAS group on two data analysis projects \
                       \to improve the resolution of Large Hadron Collier \
                       \experiments. Investigated several machine learning \
                       \techniques for jet finding and developed a jet classifier \
                       \competitive with state of the art.\nSince June 2014, \
                       \researched model based clustering for jet finding. Wrote \
                       \10k+ lines of algorithmic C++ to conduct and automate \
                       \statistical analyses that handle TBs of data and generate \
                       \plots and histograms to communicate results."

      justifiedLeader (p $ do {b "Instructor"; ", Stanford Physics 91SI" })
                      (p $ preEscapedString "March 2015 &ndash; June 2015")
      resumeParagraphs "Designed and created lecture materials, met with faculty \
                       \to plan the curriculum, and lectured weekly for Physics \
                       \91SI, scientific computing in Python, at Stanford."

      justifiedLeader (p $ b $ "FEA")
                      (p $ preEscapedString "February 2015 &ndash; present")
      resumeParagraphs "Wrote a finite element analysis in C as well as a Scheme \
                       \interpreter to generate meshes and provide high level problem \
                       \definitions to the solver. Also implemented a lexer and \
                       \a recursive descent parser combinator for reading Scheme."

      justifiedLeader (p $ b $ "Web Server")
                      (p $ preEscapedString "September 2014 &ndash; present")
      resumeParagraphs "Created a web server from the \
                       \ground up using Haskell + HTML5/CSS + JS. The site \
                       \running on it hosts completed and ongoing projects, \
                       \research, public documents, and papers. You're looking \
                       \at it!"

      justifiedLeader (p $ b $ "Chess Engine")
                      (p $ preEscapedString "June 2012 &ndash; December 2014")
      resumeParagraphs "Designed and built a chess engine in C + x86 assembly. \
                       \The engine uses alpha-beta pruning with aspiration windows, \
                       \a variety of advanced data structures, and multithreading \
                       \to assess millions of positions per second. Ran Python-\
                       \scripted tournaments to tune engine parameters with \
                       \machine learning.",


    resumeSection (h4 $ "Talks") $ do
      (mapM_ renderTalkRecord ts),

    resumeSection (h4 $ "Computer Skills") $ do
      justified (p $ i "Proficient") (p "C{++}, Python, Clojure, UNIX, LaTeX, Haskell, ROOT")
      justified (p $ i "Experienced") (p "Ruby, HTML/CSS, JavaScript, \
                                         \Mathematica/Matlab"),
    resumeSection (do
      h4 $ "Activities +"
      h4 $ "Interests")
        (p $ "Hiking, running, writing short stories, cooking")
  ]

resumeBuilder :: [Html] -> Html
resumeBuilder inserts =
  article ! A.id "resume" $ do
    H.div ! A.class_ "hruled" ! A.id "resume-header" $ do
      h3 "Conrad Stansbury"
      forM_ ["531 Lasuen Mall", "PO Box 17562", "Stanford, CA 94305"] H.p
    forM_ inserts (H.div ! A.class_ "resume-section")
