{-# LANGUAGE OverloadedStrings #-}

module Views.Fuzzy
       (fuzzyTalkView) where

import Control.Monad (msum, mapM, foldM)

import Data.Monoid (mempty)
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Time.Format
import Aux (englishJoin)

import Prelude hiding (div, span)

import Text.Blaze.Html5 hiding (map, style)
import Text.Blaze.Html5.Attributes hiding (span)
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

revealHead :: Html
revealHead =
  H.head $ do
    H.title $ "Fuzzy Jets Talk"

    meta ! charset "utf-8"
    meta ! name "description" ! content "Model Based Clustering in Jet Physics"
    meta ! name "author" ! content "Conrad Stansbury"
    meta ! name "apple-mobile-web-app-capable" ! content "yes"
    meta ! name "apple-mobile-web-app-status-bar-style" ! content "black-translucent"
    meta ! name "viewport" ! content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
      --, minimal-ui"

    link ! rel "stylesheet" ! href "resource/css/reveal.css"
    link ! rel "stylesheet" ! href "resource/css/theme/fuzzy_theme.css" ! A.id "theme"
    link ! rel "stylesheet" ! href "resource/lib/css/zenburn.css"
    link ! rel "stylesheet" ! type_ "text/css" ! href "http://fonts.googleapis.com/css?family=Merriweather+Sans:400,700,400italic,300,300italic"

    script ! A.src "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js" $ mempty

slide = section

right = style "text-align:right;"
left = style "text-align:left;"
center = style "text-align:center;"

titleSlide = slide $ do
  h1 $ do
    b $ do
      span ! style "color:transparent;text-shadow: 0 0 7px rgba(0,0,0,1);" $ "Fuzzy"
      span $ " Jets"
  p $ "Conrad Stansbury"
  div ! class_ "fragment" $ do
    i $ small $ "with"
    p $ "Benjamin Nachman + Ariel Schwartzman"

  p $ small $ "May 19, 2015"

introductionSlide = slide $ do
  h3 $ "Jet Clustering at Hadron Colliders"
  p ! class_ "fragment" $ do
    "Bridge between "
    i $ "observables"
    " and physics shortly after particle interaction"
  img ! width "600px" ! (customAttribute "data-src" "resource/img/jet.png")

seqRSlide = slide $ do
  h3 ! left $ "Jet Clustering"
  p ! right ! class_ "fragment" $ do
    "determine "
    i $ "like"
    " particles"

  p ! right ! class_ "fragment" $ "data are four-vectors"
  div ! class_ "fragment" $ do
    p ! right $ "distance metrics measure similarity"
    div ! center ! width "auto" ! style "overflow: hidden; white-space: nowrap;" $ do
      div ! style "vertical-align: middle; margin-right: 50px; display: inline-block;" $ do
        displayMath "d_{ij}, "
        p ! class_ "fragment" $ "between particles"

      div ! style "vertical-align: middle; margin-right: 50px; display: inline-block;" $ do
        displayMath "d_{iB}"
        p ! class_ "fragment" $ "to beam axis"

seqRContinuedSlide = slide $ do
  h3 ! left $ "Sequential Recombination"

  p ! center ! class_ "fragment" $ do
    "repeatedly combine close particles: "
    inlineMath "d_{ij}"

  p ! center ! class_ "fragment" $ do
    "stop when close to beam axis: "
    inlineMath "d_{iB}"

  div ! center ! width "auto" ! style "overflow: hidden; white-space: nowrap;"
      ! class_ "fragment" $ do
        img ! height "200px" ! style "margin-right:50px;" ! (customAttribute "data-src" "resource/img/kt_example.png")
        img ! height "200px" ! (customAttribute "data-src" "resource/img/antikt_example.png")

  p ! center ! class_ "fragment" $ do
    "family of algorithms arising from choice of metrics"

otherOptionsSlide = slide $ do
  h3 ! left $ "Other Options for Clustering"

  p ! right ! class_ "fragment" $ do
    "view jet clustering as "
    i "unsupervised learning"
  p ! right ! class_ "fragment" $ do
    "use statistics/ML techniques"

  div ! class_ "fragment" ! style "margin-top:100px;" $ do
    p ! center $ do
      b "Fuzzy jets"
      " views particles as observations from unknown PDF"

    p ! center $ do
      "PDF specifies jet information"

fader :: Int -> Html -> Html
fader i h = h ! class_ "fragment fade-in" ! (customAttribute "data-fragment-index") (stringValue $ show i)

fadeIn :: Int -> Html -> Html
fadeIn i h = div ! class_ "fragment fade-in" ! (customAttribute "data-fragment-index") (stringValue $ show i) $ h

fadeInInline :: Int -> Html -> Html
fadeInInline i h = span ! class_ "fragment fade-in" ! (customAttribute "data-fragment-index") (stringValue $ show i) $ h

fadeInOut :: Int -> Html -> Html
fadeInOut i h =
  div ! class_ "fragment fade-in" ! (customAttribute "data-fragment-index") (stringValue $ show i)
    $ div ! class_ "fragment fade-out none-on-out" ! (customAttribute "data-fragment-index") (stringValue $ show (i + 1)) $ h

mixtureModelingSlide = slide $ do
  h3 ! left $ "Mixture Modeling"
  div $ do
    fadeInInline 5 $
      inlineMathColored "black" "p("

    fadeInInline 5 $
      inlineMathColored "green" "\\vec{X}"

    fadeInInline 5 $
      inlineMathColored "black" ") = "

    fadeInInline 4 $
      inlineMathColored "black" "\\sum_{j=1}^{k}"

    fadeInInline 4 $
      inlineMathColored "purple" "\\pi_j"

    fadeInInline 1 $
      inlineMathColored "red" "\\Phi"

    fadeInInline 2 $
      inlineMathColored "black" "("

    fadeInInline 3 $
      inlineMathColored "green" "\\vec{X}"

    fadeInInline 2 $
      inlineMathColored "black" "\\mid"

    fadeInInline 2 $
      inlineMathColored "blue" "\\vec{\\theta}_j"

    fadeInInline 2 $
      inlineMathColored "black" ")"

  div ! style "margin-top:100px" $ do
    fadeIn 1 $ span ! left $ do
      inlineMathColored "red" "\\Phi " ! style "margin-right: 20px;"
      inlineMathColored "black" " \\equiv" ! style "margin-right: 20px;"
      "probability density"

    fadeIn 2 $ span ! left $ do
      inlineMathColored "blue" "\\vec{\\theta}_j " ! style "margin-right: 20px;"
      inlineMathColored "black" " \\equiv" ! style "margin-right: 20px;"
      "density shape parameters"

    fadeIn 3 $ span ! left $ do
      inlineMathColored "green" "\\vec{X} " ! style "margin-right: 20px;"
      inlineMathColored "black" " \\equiv" ! style "margin-right: 20px;"
      "a sample observation (4-vector)"

    fadeIn 4 $ span ! left $ do
      inlineMathColored "purple" "\\pi_j " ! style "margin-right: 20px;"
      inlineMathColored "black" " \\equiv" ! style "margin-right: 20px;"
      "component weight prior"


displayMathColored :: String -> Html -> Html
displayMathColored c h = div ! (style . stringValue) ("font-size: 80px; color:" ++ c ++ ";") ! class_ "formula display" $ h

inlineMathColored :: String -> String -> Html
inlineMathColored c h = span ! (style . stringValue) ("display: inline-block; font-size: 60px; color:" ++ c ++ ";") ! class_ "formula display" $ toHtml h

displayMath = displayMathColored "#f76a24"
inlineMath = inlineMathColored "#f76a24"

mixtureExampleSlide = slide $ do
  img ! style "margin: 0 0 -60px 0;" ! width "800px" ! (customAttribute "data-src" "resource/img/mixture.png")
  fadeIn 1 $ p "Three jets? Two jets?"

irSafetySlide = slide $ do
  fadeIn 1 $ h3 $ "Infrared (IR) Safety"
  img ! style "margin-top:60px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/ir_safe.png")

  fadeIn 1 $ p ! center $ do
    "Adding particles with "
    inlineMathColored "green" "p_T "
    inlineMathColored "black" "\\rightarrow 0"
    " should preserve jet structure"

cSafetySlide = slide $ do
  fadeIn 1 $ h3 $ "Collinear (C) Safety"
  img ! style "margin-top:50px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/c_safe.png")
  fadeIn 1 $ p ! center $ "Splitting particles should preserve jet structure"

likelihoodSlide = slide $ do
  h3 ! left $ "Likelihood"
  div $ do
    fadeInInline 1 $
      inlineMathColored "black" "L(\\{"

    fadeInInline 2 $
      inlineMathColored "green" "p_{T,i}"

    fadeInInline 2 $
      inlineMathColored "black" ","

    fadeInInline 1 $
      inlineMathColored "green" "\\rho_i"

    fadeInInline 1 $
      inlineMathColored "black" "\\mid"

    fadeInInline 1 $
      inlineMathColored "blue" "\\vec{\\theta}"

    fadeInInline 1 $
      inlineMathColored "black" "\\}) = \\sum_{i = 1}^{m}"

    fadeInInline 3 $
      inlineMathColored "green" "p_{T,i}^\\alpha"

    fadeInInline 1 $
      inlineMathColored "black" "\\sum_{j=1}^{k}"

    fadeInInline 1 $
      inlineMathColored "purple" "\\pi_j"

    fadeInInline 1 $
      inlineMathColored "red" "\\Phi"

    fadeInInline 1 $
      inlineMathColored "black" "("

    fadeInInline 1 $
      inlineMathColored "green" "\\rho_i"

    fadeInInline 1 $
      inlineMathColored "black" "\\mid"

    fadeInInline 1 $
      inlineMathColored "blue" "\\vec{\\theta}_j"

    fadeInInline 1 $
      inlineMathColored "black" ")"

  div ! style "margin-top:100px" $ do
    fadeIn 2 $ span ! left $ do
      inlineMathColored "green" "\\vec{X} " ! style "margin-right: 20px;"
      inlineMathColored "black" "\\equiv" ! style "margin-right: 20px;"
      inlineMathColored "black" "("
      inlineMathColored "green" "p_{T}"
      inlineMathColored "black" ","
      inlineMathColored "green" "\\rho"
      inlineMathColored "black" ")"

  div ! style "margin-top:60px" $ do
    fadeIn 3 $ span ! left $ do
      "Weight by particle "
      inlineMathColored "green" "p_{T}"
      "!"

pTSlide = slide $ do
  fadeIn 1 $ h3 $ do
    inlineMathColored "black" "p_T"
    " Comparison"

  div $ do
    span ! style "margin-right: 250px;" $ do
      "Anti-"
      inlineMathColored "black" "k_t"
    span $ do
      "Fuzzy"

  img ! style "margin-top:50px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/pt.png")

massTakeawaysSlide = do
  slide $ do
    h3 "Kinematics Takeaways"

    div ! style "margin-top: 150px;" $ do
      fadeIn 1 $ p "Fuzzy jets learns structural features of events"
    div ! style "margin-top: 100px;" $ do
      fadeIn 2 $ p "Learning smaller features hampers kinematics"

  slide $ do
    h3 "Kinematics Takeaways"

    div ! style "margin-top: 150px;" $ do
      p "Q: How can we use these properties to our benefit?"
    div ! style "margin-top: 100px;" $ do
      fadeIn 1 $ p "A: Learned PDF provides variables to study event."


massSlide = slide $ do
  fadeIn 1 $ h3 "Mass Comparison"

  div $ do
    span ! style "margin-right: 250px;" $ do
      "Anti-"
      inlineMathColored "black" "k_t"
    span $ do
      "Fuzzy"

  img ! style "margin-top:30px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/m.png")

wBosonSlide = slide $ do
  fadeIn 1 $ h3 "Learning W Bosons"
  div ! center ! style "margin: auto;" $ do
      div ! style "margin: 0; float: left; width: 50%;" $ do
        div $ do
          img ! style "margin-top:50px" ! width "400px"
            ! (customAttribute "data-src" "resource/img/w_ed.png")
      div ! style "margin: 0; float: left; width: 50%;" $ do
        img ! style "margin-top:60px" ! width "400px"
          ! (customAttribute "data-src" "resource/img/w_edh.png")

pileupIntroSlide = slide $ do
  h3 "Pileup"

  div ! style "margin-top: 70px;" $ fadeIn 1 $ p "Another large component of research was understanding and coping with noise"

  div ! style "margin-top: 50px;" $ fadeIn 2 $ p "Principal noise sources at LHC due to additional, unwanted collisions"

  div ! style "margin-top: 50px;" $ fadeIn 3 $ p "A major pain for doing physics!"

pileupEDSlide = slide $ do
  h3 "Pileup"

  div $ do
    img ! style "margin-top:30px" ! width "800px"
      ! (customAttribute "data-src" "resource/img/pileup.png")


pileupCorrectionSlide = slide $ do
  h3 "Proposed Solutions"

  div ! style "margin-top: 60px;" $ fadeIn 1 $ p "Some techniques promising for removing pileup before clustering"

  div ! style "margin-top: 40px;" $ fadeIn 2 $ p "Methods to remove pileup during/after clustering not applicable!"

  div ! style "margin-top: 40px;" $ fadeIn 3 $ p $ do
    "Idea: introduce \"background\" jet during clustering: "
    inlineMathColored "black" "p_{ij}\\rightarrow \\frac{p_{ij}}{\\gamma + \\sum_k p_{ik}}"

pileupCorrectedEDSlide = slide $ do
  h3 "Event Jet + Tower Subtraction"

  div $ do
    img ! style "margin-top:30px" ! width "800px"
      ! (customAttribute "data-src" "resource/img/pileup_corrected.png")

pileupHistoSlide = slide $ do
  h3 "Event Jet + Tower Subtraction"

  div $ do
    img ! style "margin-top:10px" ! width "600px"
      ! (customAttribute "data-src" "resource/img/pileup_histo.png")

sigmaIntroductionSlide = slide $ do
  h3 $ do
    span ! style "text-transform: none;" $ inlineMathColored "black" "\\sigma"
    " and Constructing Variables"

  div ! style "margin-top: 70px;" $ fadeIn 1 $ p "Can include membership probability distributions"

  div ! style "margin-top: 50px;" $ fadeIn 2 $ p $ do
    "Natural variables depend on choice of kernel, "
    inlineMathColored "red" "\\Phi"

  div ! style "margin-top: 50px;" $ fadeIn 3 $ do
    p "Studied mostly isotropic Gaussians: "
    span $ do
      span ! style "margin-right: 10px;" $ inlineMathColored "blue" "\\Sigma"
      span ! style "margin-right: 10px;" $ inlineMathColored "black" "\\equiv"
      inlineMathColored "black" "\\text{diag}("
      inlineMathColored "blue" "\\sigma^2"
      inlineMathColored "black" ","
      inlineMathColored "blue" "\\sigma^2"
      inlineMathColored "black" ")"

sigmaSlide = slide $ do
  h3 $ do
    "Leading and Average "
    span ! style "text-transform: none;" $ inlineMathColored "black" "\\sigma"

  div $ do
    span ! style "margin-right: 250px;" $ do
      "Leading "
      inlineMathColored "black" "\\sigma"
    span $ do
      "Average "
      inlineMathColored "black" "\\sigma"

  img ! style "margin-top:30px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/sigma.png")

sigmaCorrSlide = slide $ do
  h3 $ do
    "Correlations with "
    inlineMathColored "black" "\\text{m}/p_T"

  img ! style "margin-top:30px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/sigma_mass_corr.png")

  p $ do
    inlineMathColored "black" "\\sigma"
    i " is "
    "correlated... "
    fadeInInline 1 "...but..."

sigmaTaggingSlide = slide $ do
  h3 $ do
    "Tagging with  "
    span ! style "text-transform: none;" $ inlineMathColored "black" "\\sigma"

  img ! style "margin-top:30px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/tag.png")

  p "Still useful for tagging!"

safetySlide = slide $ do
  h3 ! left $ "Likelihood"
  div $ do
    inlineMathColored "black" "L(\\{"
    inlineMathColored "green" "p_{T,i}"
    inlineMathColored "black" ","
    inlineMathColored "green" "\\rho_i"
    inlineMathColored "black" "\\mid"
    inlineMathColored "blue" "\\vec{\\theta}"
    inlineMathColored "black" "\\}) = \\sum_{i = 1}^{m}"
    inlineMathColored "green" "p_{T,i}^\\alpha"
    inlineMathColored "black" "\\sum_{j=1}^{k}"
    inlineMathColored "purple" "\\pi_j"
    inlineMathColored "red" "\\Phi"
    inlineMathColored "black" "("
    inlineMathColored "green" "\\rho_i"
    inlineMathColored "black" "\\mid"
    inlineMathColored "blue" "\\vec{\\theta}_j"
    inlineMathColored "black" ")"

  div ! style "margin-top:130px" $ do
    span ! left $ do
      "Taking "
      inlineMathColored "black" "\\alpha = 1"
      " grants IRC safety"

emAlgSlide = slide $ do
  fadeIn 1 $ h3 "EM Algorithm"
  img ! style "margin-top:60px" ! width "800px"
    ! (customAttribute "data-src" "resource/img/em_alg.png")
  fadeIn 2 $ p "Iterative optimization of the likelihood"

emExampleSlide = slide $ do
  fadeIn 1 $ h3 "EM Example"
  img ! style "margin-top:60px" ! width "900px"
    ! (customAttribute "data-src" "resource/img/em_example.png")

eventDisplaySlides = do
  -- fuzzy event display
  slide $ do
    h3 "Fuzzy Event Display"
    div ! center ! width "140%" ! style "margin: auto;" $ do
      div ! style "margin-right: 50px; float: left; width: 450px;" $ do
        img ! style "margin-top:100px; padding-right: 3px;" ! width "402px"
          ! (customAttribute "data-src" "resource/img/ed.png")

      div ! style "float: left; width: auto; margin-top:100px;" $ do
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Learn structure at differing scales"
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Sensitive to different physics processes"
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Locations correspond well to classical jets"

  -- antikt event display
  slide $ do
    h3 $ do
      "Anti-"
      inlineMathColored "black" "k_t"
      " Locations"

    div ! center ! width "100%" ! style "margin: auto;" $ do
      div ! style "margin-right: 50px; float: left; width: 450px;" $ do
        img ! style "margin-top:100px" ! width "400px"
          ! (customAttribute "data-src" "resource/img/ed_antikt.png")
      div ! style "float: left; width: auto; margin-top:100px;" $ do
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Uniform in size"
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Predictable kinematics"
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Sensitive at a particular scale"

  -- voronoi event display
  slide $ do
    h3 "Voronoi Catchment Areas"
    div ! center ! width "100%" ! style "margin: auto;" $ do
      div ! style "margin-right: 50px; float: left; width: 450px;" $ do
        img ! style "margin-top:100px" ! width "400px"
          ! (customAttribute "data-src" "resource/img/ed_vor.png")
      div ! style "float: left; width: auto; margin-top:100px;" $ do
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Defined up to choice of assignment"
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Variety of shapes and sizes"
        div ! style "margin-top:50px;" $ small ! class_ "fragment" $ "Potentially unbounded areas"

conclusionsSlide = slide $ do
  h3 "Wrap Up"

  div ! style "margin-top: 50px;" $ fadeIn 1 $  "New class of clustering models for particle physics"

  div ! style "margin-top: 50px;" $ fadeIn 2 $ p $ do
    "Family parameterized by kernel choice, "
    inlineMathColored "red" "\\Phi"

  div ! style "margin-top: 50px;" $ fadeIn 3 $ p "Flexible model capable of learning event structure"

  div ! style "margin-top: 50px;" $ fadeIn 4 $ p "Improves tagging performance on Monte Carlo"


questionsSlide = slide $ do
  h2 ! style "margin-top: 150px;" $ "Questions?"

backupSlides = do
  -- weight map/ML
  slide $ do
    h3 "Particle Assignment"
    div $ do
      img ! style "margin-top:50px" ! width "800px"
          ! (customAttribute "data-src" "resource/img/weight_map.png")

  -- update step / CLL
  slide $ do
    h3 "Gaussian Update + CLL"
    div $ do
      img ! style "margin-top:80px" ! width "800px"
          ! (customAttribute "data-src" "resource/img/cll.png")

    div $ do
      img ! style "margin-top:80px" ! width "800px"
          ! (customAttribute "data-src" "resource/img/gaussian_update.png")

  -- seq recomb detail
  slide $ do
    h3 "Sequential Recombination"
    div $ do
      img ! style "margin-top:30px" ! width "800px"
          ! (customAttribute "data-src" "resource/img/seq_recomb_alg.png")

  -- pt cut
  slide $ do
    h3 $ do
      "Choosing the "
      inlineMathColored "black" "p_T"
      " Cut"

    div $ do
      span ! style "margin-right: 250px;" $ do
        inlineMathColored "black" "Z'\\rightarrow t\\bar{t}"
      span "QCD"


    div $ do
      img ! style "margin-top:30px" ! width "800px"
          ! (customAttribute "data-src" "resource/img/pt_cut.png")


fuzzyTalkView :: Html
fuzzyTalkView = docTypeHtml $ do
  revealHead
  body $ do
    div ! class_ "reveal" $
      div ! class_ "slides" $ do
        titleSlide
        introductionSlide

        seqRSlide
        seqRContinuedSlide

        otherOptionsSlide
        mixtureModelingSlide
        mixtureExampleSlide

        irSafetySlide
        cSafetySlide
        likelihoodSlide
        safetySlide

        emAlgSlide
        emExampleSlide
        eventDisplaySlides

        pTSlide
        massSlide
        wBosonSlide
        massTakeawaysSlide

        sigmaIntroductionSlide
        sigmaSlide
        sigmaCorrSlide
        sigmaTaggingSlide

        pileupIntroSlide
        pileupEDSlide
        pileupCorrectionSlide
        pileupCorrectedEDSlide
        pileupHistoSlide

        conclusionsSlide
        questionsSlide

        backupSlides

    script ! src "resource/lib/js/head.min.js" $ mempty
    script ! src "resource/js/reveal.js" $ mempty
    script ! src "resource/js/fuzzy_talk.js" $ mempty
