{-# LANGUAGE OverloadedStrings #-}

module Views.Dominion
       (dominionView) where

import Text.Blaze.Html5
import Text.Blaze.Internal (preEscapedString)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid (mempty)
import Data.List (intercalate)

import Templates.Partial (standardPartial)

dominionView :: Html
dominionView = standardPartial $ dominionContent

defaultEditorContent :: String
defaultEditorContent = ";; Example policy always forfeits turn. Write your own policy here!\n\n{:plays\n (fn [_ _]\n   {:intent :finish-turn})}"

codify :: String -> Html
codify c = H.div ! A.class_ "code-container" $ H.pre $ H.code ! A.class_ "clojure" $ toHtml c

dominionContent :: Html
dominionContent = do
  H.section $ do
    H.div ! A.id "dominion-header" $ do
      H.h1 $ "Dominion"
      p $ do
        "While teaching my "
        a ! A.href "https://web.stanford.edu/group/frg/students/sunil.html" $
          "friend, Sunil,"
        " about Clojure on the Caltrain we produced a more or less complete "
        a ! A.href "http://riograndegames.com/Game/278-Dominion" $ "Dominion"
        " simulator. I've made a few modifications to add a\
        \ set of 10 cards, and the ability to load sandboxed AIs."
      p "Included below are brief descriptions of the batteries, bells, and\
        \ whistles of the simulator, together with an editor so you can write\
        \ and run your own AI."
      p "Performance overhead from sandboxing means that I can only run a\
        \ handful of games for each submission. It's also worth pointing out\
        \ that there is a cap of 500 ms per decision for the AIs, after\
        \ which point they will forfeit their turn."

  H.section ! A.id "dominion-widget" $ do
    H.div ! A.id "dominion-editor-container" $ do
      H.div ! A.id "dominion-editor" $ toHtml defaultEditorContent

    H.button ! A.type_ "button" ! A.id "run-button" $ "Run"

  H.div ! A.id "results-div" $ mempty

  H.section $ do
    H.h1 "Simulator Information"

    p "The simulator maintains during each turn the global game state, with a structure summarized below, and information specific to a single turn, including buying power and the number of actions remaining for the current player."

    p "In order to determine what moves to make on a given turn, the simulator executes a policy, which is a collection of functions of (generally) the game state viewable from the active policy's point of view and the turn information. In order to be playable, a policy need only provide a function to decide what move to make on its turn, as reasonable defaults will be provided in the simulator utilities for reactions and other decisions."

  H.section $ do
    H.h2 "Public Game State"

    p "The global game state available to a policy (through its first argument) is a Clojure map. Its structure follows this example documenting the game state at the start of a new game on Asteria's turn:"

    codify $ intercalate "\n"
      [";; Publicly viewable state",
       "{:player-names #{\"Abas\" \"Asteria\"}",
       " :turns [\"Asteria\" \"Abas\"]",
       " :card-pool {:Province  8",
       "             :Gold      30",
       "             :Ironworks 8",
       "          ;; :Card      Count",
       "             }",
       " :players {\"Asteria\"",
       "           {:name \"Asteria\"",
       "            :hand [:Copper :Copper :Copper :Estate :Copper]",
       "            :deck {:Copper 3 :Estate 2}",
       "            :played-cards []",
       "            :discard []",
       "           \"Abas\"",
       "           {:name \"Abas\"",
       "            :hand-size 5",
       "            :hand-and-deck {:Copper 7 :Estate 3}",
       "            :played-cards []",
       "            :discard []}}}"]

  H.section $ do
    H.h2 "Turn Information"

    p "Additionally, the second argument to most functions in a policy is a description of the state of the policy's turn. This includes gold (including played treasures but excluding treasures in hand), and takes the format:"

    codify $ intercalate "\n"
      [";; Turn information",
       "{:buys    1",
       " :actions 1",
       " :gold    0",
       " :phase   :action}",
       "",
       ";; :phase can be :action or :buy"]

  H.section $ do
    H.h2 "Minimal Policy"

    p "A minimal viable policy is a map containing a function associated to the key :plays."

    p "This function takes the game state and turn information and produces a move, which should be one of:"

    codify $ intercalate "\n"
      [";; Play action",
       "{:intent  :play-action",
       " :card    :Smithy}",
       ";; the value for :card can be any action in hand"]

    codify $ intercalate "\n"
      [";; Buy a card",
       "{:intent  :buy",
       " :card    :Province}",
       ";; the value for :card can be any affordable card",
       ";; available for purchase"]

    codify $ intercalate "\n"
      [";; Finish turn",
       "{:intent  :finish-turn}"]

    p "The :plays function is repeatedly executed until a :finish-turn :intention is produced."

  link ! A.href "/resource/css/dominion.css" ! A.rel "stylesheet"
  link ! A.href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/8.6/styles/github-gist.min.css" ! A.rel "stylesheet"

  H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/8.6/highlight.min.js" $ mempty
  H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.6/languages/clojure.min.js" $ mempty

  H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js" $ mempty
  H.script ! A.src "/resource/js/dominion.js" $ mempty
