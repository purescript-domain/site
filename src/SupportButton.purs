module Domains.Site.SupportButton where

import Prelude

import CSS (StyleM, alignItems, background, black, border, boxShadow, color, display, em, fontFamily, fontSize, height, inlineFlex, justifyContent, lineHeight, margin, nil, noneTextDecoration, outline, padding, paddingLeft, paddingRight, pseudo, px, solid, star, textDecoration, white, width, (&), (?))
import CSS as CSS
import CSS.Box (bsColor, bsInset, shadowWithSpread)
import CSS.Common (center, inherit)
import CSS.Size (unitless)
import Data.NonEmpty (singleton)
import Data.Tuple (uncurry)
import Domains.Site.Theme as Theme
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

rootClass = ClassName "support-button" :: ClassName
iconWrapClass = ClassName "support-button__icon-wrap" :: ClassName
iconClass = ClassName "support-button__icon" :: ClassName
labelClass = ClassName "support-button__label" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in do
    star & byClass rootClass ? do
      border solid nil black
      outline solid nil black
      background black
      color white
      margin nil nil nil nil
      padding nil nil nil nil
      uncurry fontFamily Theme.roboto
      fontSize inherit
      lineHeight $ unitless 1.0
      textDecoration noneTextDecoration
      display inlineFlex
      alignItems center
    (star & byClass rootClass) & pseudo "focus" ? do
      boxShadow $ singleton $ bsColor Theme.gold $ bsInset $ shadowWithSpread nil nil nil $ px 1.0
    star & byClass iconWrapClass ? do
      width $ em 1.5
      height $ em 1.5
      background Theme.gold
      display inlineFlex
      alignItems center
      justifyContent center
    star & byClass iconClass ? do
      width $ em 1.0
      height $ em 1.0
    star & byClass labelClass ? do
      paddingRight $ em 0.5
      paddingLeft $ em 0.5

type URL = String

type ImageURL = String

type Label = String

supportButton :: forall p i. URL -> ImageURL -> Label -> HH.HTML p i
supportButton url imageURL label =
  HH.a
    [HP.href url, HP.target "_blank", HP.class_ rootClass]
    [ HH.div
      [ HP.class_ iconWrapClass ]
      [ HH.img [ HP.class_ iconClass, HP.src imageURL, HP.alt "" ]
      ]
    , HH.div
      [HP.class_ labelClass]
      [HH.text label]
    ]
