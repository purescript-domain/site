module PurescriPT.Site.NotFound where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

component :: forall q i o m. H.Component q i o m
component = Hooks.component \_ _ ->
  Hooks.pure $ HH.div_ [ HH.text "Not found" ]
