module Domains.Site.App where

import Prelude

import CSS (StyleM, backgroundColor, color, display, em, fontFamily, fontSize, fontWeight, inlineBlock, lineHeight, margin, marginBottom, marginLeft, marginRight, maxWidth, nil, noneTextDecoration, position, px, relative, rem, star, textDecoration, textTransform, transforms, vh, vw, white, width, (&), (?))
import CSS as CSS
import CSS.Common (auto, normal)
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import CSS.Transform (scale, translateY)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Array (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Domains.Site.Home as Home
import Domains.Site.NotFound as NotFound
import Domains.Site.Route (Route(..))
import Domains.Site.Route as Route
import Domains.Site.Terms as Terms
import Domains.Site.Theme as Theme
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import MarkdownIt (MarkdownIt)
import Type.Prelude (Proxy(..))

containerClass = ClassName "app__container" :: ClassName
headerClass = ClassName "app__header" :: ClassName
headingLinkClass = ClassName "app__heading-link" :: ClassName
headingClass = ClassName "app__heading" :: ClassName
dotClass = ClassName "app__dot" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      star & byClass containerClass ? do
        width $ vw 96.0
        maxWidth $ px 740.0
        margin (vh 8.0) auto (vh 8.0) auto
        backgroundColor Theme.darkGray
        color white
      star & byClass headerClass ? do
        marginBottom $ vh 4.0
      star & byClass headingLinkClass ? do
        textDecoration noneTextDecoration
        color white
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

component
  :: forall r i o m
   . MonadAsk { markdownIt :: MarkdownIt, markdownRef :: Ref Int | r } m
  => MonadEffect m
  => H.Component Query i o m
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
          let
            logo =
              [ HH.h1
                  [ HP.class_ headingClass ]
                  [ HH.text "purescri"
                  , HH.span [ HP.class_ dotClass ] [ HH.text "." ]
                  , HH.text "pt"
                  ]
              ]
          in
            case route of
              Just Home -> logo
              _ -> [HH.a [HP.href $ Route.print Home, HP.class_ headingLinkClass] logo]
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
