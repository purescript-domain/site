module Domains.Site.Home where

import Prelude

import CSS (StyleM, em, fontSize, marginTop, star, vh, (&), (?), (|+), (|>))
import CSS as CSS
import Domains.Site.Prose (prose)
import Domains.Site.Steps (useSteps)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

rootClass = ClassName "home" :: ClassName
introClass = ClassName "home__intro" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      star & byClass introClass ? do
        fontSize $ em 1.25
      ((star & byClass rootClass) |> star) |+ star ? do
        marginTop $ vh 4.0

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  steps <- useSteps
  Hooks.pure $
    HH.div
      [ HP.class_ rootClass ]
      [ HH.div
        [ HP.class_ introClass ]
        [ prose
          [ HH.text
              """
                PureScript Domains offers free domain names for open-source
                projects and community members. Register your name in a few
                simple steps.
              """
          ]
        ]
      , steps
      ]
