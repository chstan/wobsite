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
      justified (p $ i "Stanford University") (p "GPA 4.00")
      H.div $ p "Pursuing BS with Honors in Physics"
      H.div $ p "To be conferred in June 2015",

    resumeSection (h4 $ "Coursework") $ do
      --p $ toHtml $ (intercalate ", " relevantCoursesPhysics)
      p $ toHtml $ (intercalate ", " relevantCoursesCS)
      p $ toHtml $ (intercalate ", " relevantCoursesMath),

    resumeSection (h4 "Experience") $ do
      justifiedLeader (p $ do {b "Research Intern"; " SLAC National Laboratory" })
                      (p $ preEscapedString "June 2013 &ndash;")
      resumeParagraphs "Working with the ATLAS group on two data analysis projects \
                       \to improve the resolution of Large Hadron Collier \
                       \experiments. Investigated several machine learning \
                       \techniques for jet finding and developed a jet classifier \
                       \competitive with state of the art.\nSince June 2014, \
                       \researched model based clustering for jet finding. Wrote \
                       \10k+ lines of algorithmic C++ to conduct and automate \
                       \statistical analyses that handle TBs of data and generate \
                       \plots and histograms to communicate results.",

    resumeSection (h4 $ "Projects") $ do
      justifiedLeader (p $ b $ "Chess Engine") mempty
      resumeParagraphs  "Designed and built a chess engine in C to \
                        \explore my hobby for chess and to understand how \
                        \systems programming languages can be leveraged to build \
                        \complex applications."
      justifiedLeader (p $ b $ "Web Server") mempty
      resumeParagraphs  "Created a web server from the \
                        \ground up using Haskell + HTML5/CSS + JS to learn about \
                        \web technologies in a holistic way and to host a website \
                        \for completed and ongoing projects, research, \
                        \contact information, public documents, and papers. \
                        \You're looking at it!"
      justifiedLeader (p $ b $ "FEA") mempty
      resumeParagraphs  "Wrote a finite element analysis to learn about \
                        \computational fluid dynamics and a Scheme interpreter \
                        \to generate meshes and provide high level problem \
                        \definitions to the solver.",

    resumeSection (h4 $ "Talks") $ do
      (mapM_ renderTalkRecord ts),

    resumeSection (h4 $ "Teaching") $ do
      justifiedLeader (p $ b $ "Scientific Computing in Python") mempty
      resumeParagraphs "Lecturing Physics 91SI, a one term course at Stanford \
                       \aiming to teach natural science majors about Python, \
                       \Unix, and scientific computing as a tool for data \
                       \analysis, research, and communication. Met with faculty \
                       \to design and plan the curriculum, created student \
                       \resources and lectures, taught the semiweekly lecture \
                       \component of the course, and assessed student projects and \
                       \labs.",

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
