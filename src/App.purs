module Domains.Site.App where

import Prelude

import CSS (StyleM, alignItems, backgroundColor, color, column, display, element, em, flex, flexDirection, flexGrow, fontFamily, fontSize, fontWeight, height, inlineBlock, lineHeight, margin, marginLeft, marginRight, marginTop, maxWidth, nil, noneTextDecoration, position, px, relative, star, textDecoration, textTransform, textWhitespace, transforms, vh, vw, white, whitespaceNoWrap, width, (&), (?), (|+), (|>))
import CSS as CSS
import CSS.Common (auto, center, normal)
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import CSS.Transform (scale, translateY)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Array ((:), filter, find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (get1, get2, get3, tuple3, (/\))
import Domains.Site.Home as Home
import Domains.Site.Measure (useMeasure)
import Domains.Site.NotFound as NotFound
import Domains.Site.Route (Route(..))
import Domains.Site.Route as Route
import Domains.Site.SupportButton (supportButton)
import Domains.Site.Terms as Terms
import Domains.Site.Theme as Theme
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import MarkdownIt (MarkdownIt)
import Type.Prelude (Proxy(..))

containerClass = ClassName "app__container" :: ClassName
headerClass = ClassName "app__header" :: ClassName
headerNarrowClass = ClassName "app__header--narrow" :: ClassName
headerSpacerClass = ClassName "app__header-spacer" :: ClassName
headingLinkClass = ClassName "app__heading-link" :: ClassName
headingClass = ClassName "app__heading" :: ClassName
dotClass = ClassName "app__dot" :: ClassName
footerClass = ClassName "app__footer" :: ClassName
footerNarrowClass = ClassName "app__footer--narrow" :: ClassName
footerSpacerClass = ClassName "app__footer-spacer" :: ClassName
footerLinksClass = ClassName "app__footer-links" :: ClassName
supportButtonsClass = ClassName "app__support-button" :: ClassName

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
      ((star & byClass containerClass) |> star) |+ star ? do
        marginTop $ vh 4.0
      star & byClass headerClass ? do
        display flex
        alignItems center
        textWhitespace whitespaceNoWrap
      star & byClass headerNarrowClass ? do
        flexDirection column
      star & byClass headerSpacerClass ? do
        flexGrow 1.0
        height $ em 0.5
      star & byClass headingLinkClass ? do
        textDecoration noneTextDecoration
        color white
      star & byClass headingClass ? do
        position relative
        margin nil (em 0.5) nil nil
        uncurry fontFamily Theme.montserrat
        fontSize $ em 2.5
        fontWeight normal
        textTransform uppercase
        lineHeight $ unitless 1.0
      star & byClass dotClass ? do
        display inlineBlock
        transforms [ translateY $ em (-0.6), scale 2.0 2.0 ]
        marginRight $ em 0.125
        marginLeft $ em 0.125
        color Theme.gold
      star & byClass supportButtonsClass ? do
        height $ em 2.5
        display flex
        alignItems center
      ((star & byClass supportButtonsClass) |> star) |+ star ? do
        marginLeft $ em 0.5
      star & byClass footerClass ? do
        display flex
        alignItems center
        uncurry fontFamily Theme.roboto
        fontSize $ em 0.75
        textWhitespace whitespaceNoWrap
        element "a" ? do
          color Theme.gold
      star & byClass footerSpacerClass ? do
        flexGrow 1.0
        height $ em 1.0
      (star & byClass footerClass) |> star ? do
        display inlineBlock
      star & byClass footerNarrowClass ? do
        flexDirection column
      ((star & byClass footerLinksClass) |> star) |+ star ? do
        display inlineBlock
        marginLeft $ em 0.5

data Query a = Navigate (Maybe Route) a

type Slots =
  ( header :: forall q. H.Slot q Void Unit
  , main :: forall q. H.Slot q Void (Maybe Route)
  , footer :: forall q. H.Slot q Void Unit
  )

_header = Proxy :: Proxy "header"
_main = Proxy :: Proxy "main"
_footer = Proxy :: Proxy "footer"

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
      [ HH.slot_ _header unit header route
      , HH.main_
          [ unit # uncurry (HH.slot_ _main) (fromMaybe notFound $ find (\(route' /\ _) -> route == route') pages)
          ]
      , HH.slot_ _footer unit footer route
      ]

  header = Hooks.component \_ route -> Hooks.do
    headerMeasure <- useMeasure
    headingMeasure <- useMeasure
    buttonsMeasure <- useMeasure
    Hooks.pure $
      case tuple3 <$> headerMeasure <*> headingMeasure <*> buttonsMeasure of
        Nothing ->
          HH.header_ []
        Just measures ->
          let
            headerRef /\ headerRect = get1 measures
            headingRef /\ headingRect = get2 measures
            buttonsRef /\ buttonsRect = get3 measures
          in
            HH.header
              [ HP.ref headerRef, HP.classes $ headerClass : filter (const $ headerRect.width < headingRect.width + buttonsRect.width) [ headerNarrowClass ] ] $
              [ let
                  logo =
                    HH.h1
                      [ HP.ref headingRef, HP.class_ headingClass ]
                      [ HH.text "purescri"
                      , HH.span [ HP.class_ dotClass ] [ HH.text "." ]
                      , HH.text "pt"
                      ]
                in
                  case route of
                    Just Home -> logo
                    _ -> HH.a [ HP.href $ Route.print Home, HP.class_ headingLinkClass ] [ logo ]
              , HH.div [ HP.class_ headerSpacerClass ] []
              , HH.div
                  [ HP.ref buttonsRef, HP.class_ supportButtonsClass ] $
                  (\x -> HH.div_ [ x ]) <$>
                    [ supportButton
                        "https://twitter.com/intent/tweet?url=https%3A%2F%2Fpurescri.pt"
                        "./twitter.svg"
                        "Share"
                    , supportButton
                        "https://github.com/purescript-domains/dns"
                        "./github.svg"
                        "Star"
                    , supportButton
                        "https://github.com/sponsors/purescript-domains"
                        "./sponsor.svg"
                        "Sponsor"
                    ]
              ]

  footer = Hooks.component \_ route -> Hooks.do
    footerMeasure <- useMeasure
    copyrightMeasure <- useMeasure
    linksMeasure <- useMeasure
    Hooks.pure $
      case tuple3 <$> footerMeasure <*> copyrightMeasure <*> linksMeasure of
        Nothing ->
          HH.footer_ []
        Just measures ->
          let
            footerRef /\ footerRect = get1 measures
            copyrightRef /\ copyrightRect = get2 measures
            linksRef /\ linksRect = get3 measures
          in
            HH.footer
              [ HP.ref footerRef, HP.classes $ footerClass : filter (const $ footerRect.width < copyrightRect.width + linksRect.width) [ footerNarrowClass ] ]
              [ HH.div [ HP.ref copyrightRef ] [ HH.text "Copyright © 2022 PureScript Domains" ]
              , HH.div [ HP.class_ footerSpacerClass ] []
              , HH.div
                  [ HP.ref linksRef, HP.class_ footerLinksClass ] $
                  (filter (const $ route /= Just Terms) [ HH.a [ HP.href $ "#" <> Route.print Terms ] [ HH.text "Terms and Conditions" ] ]) <>
                    [ HH.a
                        [ HP.href "https://github.com/purescript-domains", HP.target "_blank" ]
                        [ HH.text "GitHub" ]
                    , HH.a
                        [ HP.href "https://twitter.com/pursdomains", HP.target "_blank" ]
                        [ HH.text "Twitter" ]
                    ]
              ]

  pages =
    [ Just Home /\ Home.component
    , Just Terms /\ Terms.component
    ]

  notFound = Nothing /\ NotFound.component
