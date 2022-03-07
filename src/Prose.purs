module Domains.Site.Prose where

import Prelude

import CSS (StyleM, color, element, em, fontFamily, fontStyle, lineHeight, margin, marginTop, nil, star, (&), (?), (|+), (|>))
import CSS as CSS
import CSS.Common (normal)
import CSS.Size (unitless)
import Data.Tuple (uncurry)
import Domains.Site.Theme as Theme
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

rootClass = ClassName "prose" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in do
    star & byClass rootClass ? do
      uncurry fontFamily Theme.roboto
      lineHeight $ unitless 1.5
      element "em" ? do
        fontStyle normal
        color Theme.gold
      element "code" ? do
        uncurry fontFamily Theme.inconsolata
    (star & byClass rootClass) |> star ? do
      margin nil nil nil nil
    ((star & byClass rootClass) |> star) |+ star ? do
      marginTop $ em 0.75

prose :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
prose = HH.section [HP.class_ rootClass]
