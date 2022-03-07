module Domains.Site.App where

import Prelude

import CSS (StyleM, backgroundColor, color, display, em, fontFamily, fontSize, fontWeight, inlineBlock, lineHeight, margin, marginBottom, marginLeft, marginRight, maxWidth, nil, position, px, relative, rem, star, textTransform, transforms, vh, white, (&), (?))
import CSS as CSS
import CSS.Common (auto, normal)
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import CSS.Transform (scale, translateY)
import Data.Array (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Domains.Site.Home as Home
import Domains.Site.NotFound as NotFound
import Domains.Site.Route (Route(..))
import Domains.Site.Terms as Terms
import Domains.Site.Theme as Theme
import Type.Prelude (Proxy(..))

containerClass = ClassName "app__container" :: ClassName
headerClass = ClassName "app__header" :: ClassName
headingClass = ClassName "app__heading" :: ClassName
dotClass = ClassName "app__dot" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      star & byClass containerClass ? do
        maxWidth $ px 740.0
        margin (vh 8.0) auto (vh 8.0) auto
        backgroundColor Theme.darkGray
        color white
      star & byClass headerClass ? do
        marginBottom $ vh 4.0
      star & byClass headingClass ? do
        position relative
        margin nil nil nil nil
        uncurry fontFamily Theme.montserrat
        fontSize (rem 2.5)
        fontWeight normal
        textTransform uppercase
        lineHeight $ unitless 1.0
      star & byClass dotClass ? do
        display inlineBlock
        transforms [ translateY $ em (-0.6), scale 2.0 2.0 ]
        marginRight $ em 0.125
        marginLeft $ em 0.125
        color Theme.gold

data Query a = Navigate (Maybe Route) a

type Slots = (main :: forall q. H.Slot q Void (Maybe Route))

_main = Proxy :: Proxy "main"

component :: forall i o m. MonadEffect m => H.Component Query i o m
component =
  H.mkComponent
    { initialState: const Nothing
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery }
    , render
    }
  where

  handleQuery :: forall action a. Query a -> H.HalogenM (Maybe Route) action Slots o m (Maybe a)
  handleQuery =
    case _ of
      Navigate route next -> do
        H.put route
        pure $ Just next

  render route =
    HH.div
      [ HP.class_ containerClass ]
      [ HH.header
          [ HP.class_ headerClass ]
          [ HH.h1
              [ HP.class_ headingClass ]
              [ HH.text "purescri"
              , HH.span [ HP.class_ dotClass ] [ HH.text "." ]
              , HH.text "pt"
              ]
          ]
      , HH.main_
          [ unit # uncurry (HH.slot_ _main) (fromMaybe notFound $ find (\(route' /\ _) -> route == route') pages)
          ]
      ]

    where

    pages =
      [ Just Home /\ Home.component
      , Just Terms /\ Terms.component
      ]

    notFound = Nothing /\ NotFound.component
