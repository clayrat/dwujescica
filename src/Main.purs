module Main where

import Prelude (class Applicative, id, ($), (<$>), (<<<), (>>>), (++), const)

import Data.Array (zip)
import Data.Char (toString)
import Data.Foldable (foldl, mconcat)
import Data.Lens.Types (LensP)
import Data.Lens.Lens (lens)
import Data.Maybe (maybe)
import Data.String (toCharArray)
import Data.String.Regex as R
import Data.Tuple (Tuple, fst, snd)

import OpticUI (Markup, UI, with, withView, animate, text, ui, updatePure)
import OpticUI.Markup.HTML as H

replaceAll :: String -> String -> String -> String
replaceAll pattern repl s = R.replace (R.regex pattern gFlags) repl s where
 gFlags = { global     : true
          , ignoreCase : false
          , multiline  : false
          , sticky     : false
          , unicode    : false }

transMap :: Array (Tuple String String)
transMap = zip (toString <$> toCharArray "aąbcdeęfghiïjkłmnopqrstuvwxyzżAĄBCDEĘFGHIÏJKŁMNOPQRSTUVWXYZŻ")
               (toString <$> toCharArray "аѫбцдеѧфгхиійклмнопкрстуввхызжАѪБЦДЕѦФГХИІЙКЛМНОПКРСТУВВХЫЗЖ")

transliterate :: String -> String
transliterate s = foldl (\s' t -> replaceAll (fst t) (snd t) s') s transMap

transcribe :: String -> String
transcribe =
 replaceAll "\\b([aA]d|[bB]ez|[kK]on|[nN]ad|[oO]b|[oO]d|[pP]od|[pP]ół|[pP]onad|[pP]rzed|[pP]rzeciw|[rR]oz|[sS]ub|[wW]|[wW]spół|[wW]szech|[wW]z|[zZ])j"
                             "$1ъj"                                  >>>
 replaceAll "dzi([aąeęoóu])" "dь$1"                                  >>>
 replaceAll "Dzi([aąeęoóu])" "Dь$1"                                  >>>
 replaceAll "ci([aąeęoóu])" "tь$1"                                   >>>
 replaceAll "Ci([aąeęoóu])" "Tь$1"                                   >>>
 replaceAll "l([aąeęoóu])" "łь$1"                                    >>>
 replaceAll "L([aąeęoóu])" "Łь$1"                                    >>>
 replaceAll "di([aąeęoóu])" "dïj$1"                                  >>>
 replaceAll "Di([aąeęoóu])" "Dïj$1"                                  >>>
 replaceAll "li([aąeęoóu])" "łьj$1"                                  >>>
 replaceAll "Li([aąeęoóu])" "Łьj$1"                                  >>>
 replaceAll "ri([aąeęoóu])" "rïj$1"                                  >>>
 replaceAll "Ri([aąeęoóu])" "Rïj$1"                                  >>>
 replaceAll "ti([aąeęoóu])" "tïj$1"                                  >>>
 replaceAll "Ti([aąeęoóu])" "Tïj$1"                                  >>>
 replaceAll "di" "dï"                                                >>>
 replaceAll "ri" "rï"                                                >>>
 replaceAll "ti" "tï"                                                >>>
 replaceAll "cj" "cïj"                                               >>>
 replaceAll "dj" "dïj"                                               >>>
 replaceAll "nj" "nïj"                                               >>>
 replaceAll "rj" "rïj"                                               >>>
 replaceAll "sj" "sïj"                                               >>>
 replaceAll "tj" "tïj"                                               >>>
 replaceAll "zj" "zïj"                                               >>>
 replaceAll "([bdfghkłmnprstwzBDFGHKŁMNPRSTWZ])i([aąeęoóu])" "$1ь$2" >>>
 replaceAll "dź" "dь"                                                >>>
 replaceAll "rz" "rь"                                                >>>
 replaceAll "Rz" "Rь"                                                >>>
 replaceAll "ć" "tь"                                                 >>>
 replaceAll "l" "łь"                                                 >>>
 replaceAll "L" "Łь"                                                 >>>
 replaceAll "ń" "nь"                                                 >>>
 replaceAll "ś" "sь"                                                 >>>
 replaceAll "Ś" "Sь"                                                 >>>
 replaceAll "ź" "zь"                                                 >>>
 replaceAll "dzi" "di"                                               >>>
 replaceAll "Dzi" "Di"                                               >>>
 replaceAll "rьy" "ri"                                               >>>
 replaceAll "ci" "ti"                                                >>>
 replaceAll "ьji" "ьi"                                               >>>
 replaceAll "ï?ji" "ïi"                                              >>>
 replaceAll "łьi" "łi"                                               >>>
 replaceAll "Łьi" "Łi"                                               >>>
 replaceAll "([dtsz])ь([bdfghkłmnprstwz][iь])" "$1$2"                >>>
 replaceAll "łьł([iь])" "łł$1"                                       >>>
 replaceAll "([bcdfghjkłmnprstwzż])j" "$1ьj"                         >>>
 replaceAll "[ьj]a" "я"                                              >>>
 replaceAll "Ja|ьA" "Я"                                              >>>
 replaceAll "[ьj]ą" "ѭ"                                             >>>
 replaceAll "Ją|ьĄ" "Ѭ"                                             >>>
 replaceAll "[ьj]e" "є"                                              >>>
 replaceAll "Je|ьE" "Є"                                              >>>
 replaceAll "[ьj]ę" "ѩ"                                              >>>
 replaceAll "Ję|ьĘ" "Ѩ"                                             >>>
 replaceAll "[ьj]o" "є̈"                                              >>>
 replaceAll "Jo|ьO" "Є̈"                                              >>>
 replaceAll "[ьj]ó" "є́"                                              >>>
 replaceAll "Jó|ьÓ" "Є́"                                              >>>
 replaceAll "[ьj]u" "ю"                                              >>>
 replaceAll "Ju|ьU" "Ю"                                              >>>
 replaceAll "ch" "х"                                                 >>>
 replaceAll "Ch" "Х"                                                 >>>
 replaceAll "szcz" "щ"                                               >>>
 replaceAll "Szcz" "Щ"                                               >>>
 replaceAll "cz" "ч"                                                 >>>
 replaceAll "Cz" "Ч"                                                 >>>
 replaceAll "sz" "ш"                                                 >>>
 replaceAll "Sz" "Ш"                                                 >>>
 transliterate

translens :: LensP String String                  -- TODO use a GetterP
translens = lens transcribe const

textAreaField :: forall eff m k. (Applicative m) => String -> Boolean -> UI eff m k Markup String String
textAreaField title ro = with $ \s h -> let
  inpH _ = updatePure h <<< maybe "" id
  in ui $ H.div [H.styleA "width: 50%;"] $ mconcat
  [ H.p_ $ text title
  , H.textarea ([H.styleA "max-width: 95%; width: 95%; height: 300px", H.readonlyA ro, H.onInput inpH]) $ text s
  ]

main = animate "" $ with \s h -> mconcat
  [ ui $ H.h1_ $ text "Polish cyrillic transliterator (Dwujescica)"
  , ui $ H.h4_ (text "idea by " ++ (H.a [H.hrefA "https://wiki.lingvoforum.net/wiki/index.php/Участник:Hellerick/Польская_кириллица"] $ text "Hellerick"))
  , withView (H.div [H.styleA "display:flex;"]) $ mconcat
   [             textAreaField "Łacinka"  false
   , translens $ textAreaField "Цырылица" true
   ]
  ]
