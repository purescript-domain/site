module PurescriPT.Site.Home where

import Prelude

import CSS (StyleM, marginTop, star, vh, (&), (?), (|+), (|>))
import CSS as CSS
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import PurescriPT.Site.Steps (useSteps)

rootClass = ClassName "home" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      ((star & byClass rootClass) |> star) |+ star ? do
        marginTop $ vh 4.0

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  steps <- useSteps
  Hooks.pure $ HH.div [ HP.class_ rootClass ] [ steps ]
