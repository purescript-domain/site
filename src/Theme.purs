module PurescriPT.Site.Theme where

import CSS (Color, GenericFontFamily, rgb, sansSerif)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))

darkGray = rgb 27 31 40 :: Color

gold = rgb 188 138 51 :: Color

logoFont :: Array String /\ NonEmpty Array GenericFontFamily
logoFont = [ "Montserrat" ] /\ (sansSerif :| [])

sans :: Array String /\ NonEmpty Array GenericFontFamily
sans = [ "Roboto" ] /\ (sansSerif :| [])
