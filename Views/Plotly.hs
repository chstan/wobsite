{-# LANGUAGE OverloadedStrings #-}

module Views.Plotly
       (plotlyView

       ) where

import Data.Monoid (mempty)
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Time.Format
import Aux (englishJoin)

import Text.Blaze.Html5
import Text.Blaze (stringValue)
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data CondensedPaper =
  CondensedPaper {
    title :: String,
    authors :: [String],
    citations :: Int,
    publicationDate :: Day,
    synopsis :: String
    }

renderAbstract :: Html -> Html
renderAbstract abs =
  H.div ! A.class_ "row abstract" $ do
          H.div ! A.class_ "one columns force" $ mempty
          H.div ! A.class_ "ten columns" $ abs
          H.div ! A.class_ "one columns force" $ mempty

renderAuthors :: [String] -> Html
renderAuthors as = H.div ! A.class_ "row" $ do
  H.div ! A.class_ "two columns force" $ mempty
  H.div ! A.class_ "eight columns" $ do
    H.strong $ toHtml $ englishJoin as
  H.div ! A.class_ "two columns force" $ mempty

plotlyFrame :: Int -> Int -> String -> Html
plotlyFrame w h url = do
  H.iframe ! A.width (stringValue $ show w)
           ! A.height (stringValue $ show h)
           ! (customAttribute "frameborder" "0")
           ! (customAttribute "seamless" "seamless")
           ! (customAttribute "scrolling" "no")
           ! A.src (stringValue $ url ++ ".embed?width="
                    ++ (show w) ++ "&height=" ++ (show h))
    $ mempty

plotlyView :: Html
plotlyView = docTypeHtml $ do
  H.head $ do
    H.title "Plotly Test"
    link ! A.href "/resource/css/skeleton.css"
         ! A.rel "stylesheet" ! A.type_ "text/css"
    link ! A.href "/resource/css/normalize.css"
         ! A.rel "stylesheet" ! A.type_ "text/css"
    link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600"
         ! A.rel "stylesheet" ! A.type_ "text/css"
    link ! A.href "/resource/css/paper_custom.css"
         ! A.rel "stylesheet" ! A.type_ "text/css"
    script ! A.src "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js" $ mempty
    script ! A.src "/resource/js/paper_custom.js" $ mempty
    script ! A.src "/resource/js/textures.js" $ mempty
    script ! A.src "/resource/js/underscore.js" $ mempty
    script ! A.src "http://d3js.org/d3.v3.min.js" ! A.charset "utf-8" $ mempty
    script ! A.src "http://d3js.org/topojson.v1.min.js" $ mempty

  H.body $ do
    H.div ! A.class_ "nav" $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "logo" $ do
          H.div ! A.class_ "logo-short" $ mempty
          H.div ! A.class_ "logo-medium" $ mempty
          H.div ! A.class_ "logo-tall" $ mempty
          H.div ! A.class_ "logo-medium" $ mempty
          H.div ! A.class_ "logo-short" $ mempty
        H.div ! A.class_ "logo-text" $ do
          H.h1 "Journal"

    H.div ! A.class_ "container" $ do
      H.section ! A.class_ "header" $ do
        H.h2 ! A.class_ "title" $ "Fuzzy Jets"
        renderAuthors ["Benjamin Nachman", "Ariel Schwartzman", "Conrad Stansbury"]
        renderAbstract $ abstractText

      H.div ! A.class_ "row" $ do
        H.div ! A.class_ "twelve columns" $ do
          H.h2 "Introduction"
          do
            "As the result of a proton-proton collision at the Large \
            \Hadron Collider (LHC), hundreds particles are created and detected "

            citeButton 1
            citeButton 2

            ". "

            citationBox 1 $
              CondensedPaper "Charged-particle multiplicities in pp interactions \
                             \measured with the ATLAS detector at the LHC"
                             ["The ATLAS Collaboration"] 293
                             (fromGregorian 2011 2 8)
                             "Measurements are presented from proton-proton collisions at centre-of-mass energies of sqrt(s) = 0.9, 2.36 and 7 TeV recorded with the ATLAS detector at the LHC. Events were collected using a single-arm minimum-bias trigger. The charged-particle multiplicity, its dependence on transverse momentum and pseudorapidity and the relationship between the mean transverse momentum and charged-particle multiplicity are measured. Measurements in different regions of phase-space are shown, providing diffraction-reduced measurements as well as more inclusive ones. The observed distributions are corrected to well-defined phase-space regions, using model-independent corrections. The results are compared to each other and to various Monte Carlo models, including a new AMBT1 PYTHIA 6 tune. In all the kinematic regions considered, the particle multiplicities are higher than predicted by the Monte Carlo models. The central charged-particle multiplicity per event and unit of pseudorapidity, for tracks with pT >100 MeV, is measured to be 3.483 +- 0.009 (stat) +- 0.106 (syst) at sqrt(s) = 0.9 TeV and 5.630 +- 0.003 (stat) +- 0.169 (syst) at sqrt(s) = 7 TeV."

            citationBox 2 $
              CondensedPaper "Charged particle multiplicities in pp interactions \
                             \at sqrt(s) = 0.9, 2.36, and 7 TeV"
                             ["The CMS Collaboration"] 252
                             (fromGregorian 2010 11 24)
                             "Measurements of primary charged hadron multiplicity distributions are presented for non-single-diffractive events in proton-proton collisions at centre-of-mass energies of sqrt(s) = 0.9, 2.36, and 7 TeV, in five pseudorapidity ranges from |eta|<0.5 to |eta|<2.4. The data were collected with the minimum-bias trigger of the CMS experiment during the LHC commissioning runs in 2009 and the 7 TeV run in 2010. The multiplicity distribution at sqrt(s) = 0.9 TeV is in agreement with previous measurements. At higher energies the increase of the mean multiplicity with sqrt(s) is underestimated by most event generators. The average transverse momentum as a function of the multiplicity is also presented. The measurement of higher-order moments of the multiplicity distribution confirms the violation of Koba-Nielsen-Olesen scaling that has been observed at lower energies."

            "While some particles can be identified by their type, such as electrons "

            citeButton 3
            citeButton 4

            " "

            citationBox 3 $
              CondensedPaper "Performance of electron reconstruction and selection \
                             \with the CMS detector in proton-proton collisions \
                             \at sqrt(s)=8 TeV"
                             ["The CMS Collaboration"] 252
                             (fromGregorian 2015 2 9)
                             "The performance and strategies used in electron reconstruction and selection at CMS are presented based on data corresponding to an integrated luminosity of 19.7 inverse femtobarns, collected in proton-proton collisions at sqrt(s) = 8 TeV at the CERN LHC. The paper focuses on prompt isolated electrons with transverse momenta ranging from about 5 to a few 100 GeV. A detailed description is given of the algorithms used to cluster energy in the electromagnetic calorimeter and to reconstruct electron trajectories in the tracker. The electron momentum is estimated by combining the energy measurement in the calorimeter with the momentum measurement in the tracker. Benchmark selection criteria are presented, and their performances assessed using Z, Upsilon, and J/psi decays into electron-positron pairs. The spectra of the observables relevant to electron reconstruction and selection as well as their global efficiencies are well reproduced by Monte Carlo simulations. The momentum scale is calibrated with an uncertainty smaller than 0.3%. The momentum resolution for electrons produced in Z boson decays ranges from 1.7 to 4.5%, depending on electron pseudorapidity and energy loss through bremsstrahlung in the detector material."

            citationBox 4 $
              CondensedPaper "Electron and photon energy calibration with the ATLAS detector using LHC Run 1 data"
                             ["The ATLAS Collaboration"] 252
                             (fromGregorian 2014 11 13)
                             "This paper presents the electron and photon energy calibration achieved with the ATLAS detector using about 25 fb-1 of LHC proton--proton collision data taken at centre-of-mass energies of sqrt(s) = 7 and 8 TeV. The reconstruction of electron and photon energies is optimised using multivariate algorithms. The response of the calorimeter layers is equalised in data and simulation, and the longitudinal profile of the electromagnetic showers is exploited to estimate the passive material in front of the calorimeter and reoptimise the detector simulation. After all corrections, the Z resonance is used to set the absolute energy scale. For electrons from Z decays, the achieved calibration is typically accurate to 0.05% in most of the detector acceptance, rising to 0.2% in regions with large amounts of passive material. The remaining inaccuracy is less than 0.2-1% for electrons with a transverse energy of 10 GeV, and is on average 0.3% for photons. The detector resolution is determined with a relative inaccuracy of less than 10% for electrons and photons up to 60 GeV transverse energy, rising to 40% for transverse energies above 500 GeV."

            "and muons "

            citeButton 5
            citeButton 6

            ", "

            citationBox 5 $
              CondensedPaper "Performance of CMS muon reconstruction in pp collision events at sqrt(s) = 7 TeV"
                             ["The CMS Collaboration"] 242
                             (fromGregorian 2013 3 20)
                             "The performance of muon reconstruction, identification, and triggering in CMS has been studied using 40 inverse picobarns of data collected in pp collisions at sqrt(s) = 7 TeV at the LHC in 2010. A few benchmark sets of selection criteria covering a wide range of physics analysis needs have been examined. For all considered selections, the efficiency to reconstruct and identify a muon with a transverse momentum pT larger than a few GeV is above 95% over the whole region of pseudorapidity covered by the CMS muon system, abs(eta) < 2.4, while the probability to misidentify a hadron as a muon is well below 1%. The efficiency to trigger on single muons with pT above a few GeV is higher than 90% over the full eta range, and typically substantially better. The overall momentum scale is measured to a precision of 0.2% with muons from Z decays. The transverse momentum resolution varies from 1% to 6% depending on pseudorapidity for muons with pT below 100 GeV and, using cosmic rays, it is shown to be better than 10% in the central region up to pT = 1 TeV. Observed distributions of all quantities are well reproduced by the Monte Carlo simulation."

            citationBox 6 $
              CondensedPaper "Measurement of the muon reconstruction performance of the ATLAS detector using 2011 and 2012 LHC proton-proton collision data"
                             ["The ATLAS Collaboration"] 75
                             (fromGregorian 2014 12 7)
                             "This paper presents the performance of the ATLAS muon reconstruction during the LHC run with pp collisions at s√=7−8 TeV in 2011-2012, focusing mainly on data collected in 2012. Measurements of the reconstruction efficiency and of the momentum scale and resolution, based on large reference samples of J/ψ→μμ, Z→μμ and Υ→μμ decays, are presented and compared to Monte Carlo simulations. Corrections to the simulation, to be used in physics analysis, are provided. Over most of the covered phase space (muon |η|<2.7 and 5≲pT≲100 GeV) the efficiency is above 99% and is measured with per-mille precision. The momentum resolution ranges from 1.7% at central rapidity and for transverse momentum pT≃10 GeV, to 4% at large rapidity and pT≃100 GeV. The momentum scale is known with an uncertainty of 0.05% to 0.2% depending on rapidity. A method for the recovery of final state radiation from the muons is also presented."

            "most of the detected particles are light hadrons produced in collimated sprays called jets. Physically, jets are the consequence of high energy quarks or gluons fragmenting into colorless hadrons. Experimentally, jets are defined by clustering scheme which group together measured calorimeter energy deposits or reconstructed charged particle tracks. A jet algorithm is a clustering scheme that connects the measured objects with theoretical quantities that can be calculated and simulated. At a hadron collider, the natural coordinates for describing particles are pT , y, and phi, where pT is the magnitude of the momentum transverse to the proton beam, y is the rapidity, and phi is the azimuthal angle. Particles or calorimeter energy deposits are clustered using jet algorithms based on similarity measures on their coordinates in (pT , ρ) = (pT , y, phi). In order for a jet algorithm to be useful to experimentalists and theorists, the collection of jets should be IRC safe in the following sense:"

      --H.div ! A.class_ "row" $ do
      --  H.div ! A.class_ "twelve columns" $ do
      --    plotlyFrame 800 400 "http://plot.ly/~jackp/1841/aapl-vs-moving-average/"

      H.div ! A.class_ "row" $ do
        H.div ! A.class_ "twelve columns" $ do
          H.div ! A.class_ "textures-test" $ do mempty


citeButton :: Int -> Html
citeButton n =
  H.button ! A.class_ "citation-button"
           ! customAttribute "citation-number" (stringValue $ show n) $ do
    H.cite ! A.class_ "collapsed-citation" $ toHtml $ show n


citationBox :: Int -> CondensedPaper -> Html
citationBox n c = do
  H.span ! A.class_ "citation"
         ! customAttribute "citation-number" (stringValue $ show n) $ do
    H.span ! A.class_ "row" $ do
      H.span ! A.class_ "citation-content twelve columns" $ do
        H.span ! A.class_ "citation-header" $ do
          H.h1 $ toHtml $ Views.Plotly.title c
          H.strong $ toHtml $ englishJoin $ authors c
          H.p $ toHtml $ "Cited by " ++ (show $ citations c)
          H.p $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y" (publicationDate c)
        H.span ! A.class_ "citation-abstract" $ do
          H.blockquote $ toHtml $ (synopsis c)

abstractText = do
  "Collimated streams of particles produced in high energy physics \
  \experiments are organized using clustering algorithms to form "
  em $ "jets. "

  --citeButton 1
  --citeButton 2
  --
  --citationBox 1 $
  --  CondensedPaper "Successive combination jet algorithm for hadron collisions"
  --                 ["Stephen D. Ellis", "Davision E. Soper"] 1491
  --                 (fromGregorian 1993 5 14)
  --                 "Jet finding algorithms, as they are used in electron-positron \
  --                 \and hadron collisions, are reviewed and compared. \
  --                 \It is suggested that a successive combination style \
  --                 \algorithm, similar to that used in electron-positron physics, \
  --                 \might be useful also in hadron collisions, where cone \
  --                 \style algorithms have been used previously."
  --
  --citationBox 2 $
  --  CondensedPaper "Better jet clustering algorithms"
  --                 ["Y. L. Dokshitzer", "G. Leder", "S. Moretti", "B. Webber"] 713
  --                 (fromGregorian 1997 7 11)
  --                 "We investigate modifications to the kt-clustering \
  --                 \jet algorithm which preserve the advantages of the \
  --                 \original Durham algorithm while reducing non-perturbative \
  --                 \corrections and providing better resolution of jet \
  --                 \substructure. We find that a simple change in the \
  --                 \sequence of clustering (combining smaller-angle pairs \
  --                 \first), together with the `freezing' of soft resolved \
  --                 \jets, has beneficial effects."

  "To construct jets, the experimental collaborations based at the \
  \Large Hadron Collider (LHC) exclusively use agglomerative hierarchical \
  \clustering schemes known as sequential recombination. We propose a \
  \new class of algorithms for clustering jets that use infrared and \
  \collinear safe mixture models. These new algorithms, known as "

  em $ "fuzzy jets"

  ", are clustered using maximum likelihood techniques and can \
  \learn various properties of jets like their size. We show that \
  \the fuzzy jet size adds additional information to standard jet \
  \tagging variables. Furthermore, we study the impact of pileup \
  \and show that with some slight modifications to the algorithm, \
  \fuzzy jets can be stable up to high pileup interaction multiplicities."
