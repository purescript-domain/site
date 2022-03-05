module PurescriPT.Site.Theme where

import CSS (Color, GenericFontFamily, rgb, sansSerif)
import CSS.Font (monospace)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))

darkGray = rgb 27 31 40 :: Color

darkerGray = rgb 13 13 18 :: Color

lightGold = rgb 246 204 128 :: Color

gold = rgb 188 138 51 :: Color

montserrat :: Array String /\ NonEmpty Array GenericFontFamily
montserrat = [ "Montserrat" ] /\ (sansSerif :| [])

inconsolata :: Array String /\ NonEmpty Array GenericFontFamily
inconsolata = [ "Inconsolata" ] /\ (monospace :| [])

roboto :: Array String /\ NonEmpty Array GenericFontFamily
roboto = [ "Roboto" ] /\ (sansSerif :| [])
