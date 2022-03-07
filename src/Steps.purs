module Domains.Site.Steps where

import Prelude

import CSS (StyleM, alignItems, backgroundColor, black, border, boxShadow, color, deg, display, element, em, flex, fontFamily, fontSize, fontStyle, height, hover, inlineBlock, lineHeight, margin, marginLeft, marginTop, nil, opacity, outlineColor, outlineOffset, outlineStyle, outlineWidth, padding, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, pseudo, px, rem, rgba, rotate, solid, star, textTransform, transform, transitionDuration, transitionProperties, transitionProperty, white, width, (&), (?), (@-@), (|+), (|>))
import CSS as CSS
import CSS.Box (bsColor, bsInset, shadowWithBlur, shadowWithSpread)
import CSS.Common (center, hidden, normal)
import CSS.Display (visibility)
import CSS.Overflow (overflow)
import CSS.Overflow as Overflow
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import CSS.Transform (translateY)
import Data.Array (catMaybes, mapWithIndex, (:))
import Data.Foldable (find)
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (singleton)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Headless.Accordion (UseAccordion, useAccordion)
import Halogen.Headless.Accordion as Accordion
import Halogen.Hooks (type (<>), Hook, HookM, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Svg.Attributes (CommandPositionReference(..), l, m)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HSE
import Domains.Site.Theme as Theme

headingClass = ClassName "steps__heading" :: ClassName
triggerClass = ClassName "steps__trigger" :: ClassName
indicatorClass = ClassName "steps__indicator" :: ClassName
indicatorOpenClass = ClassName "steps__indicator--open" :: ClassName
panelClass = ClassName "steps__panel" :: ClassName
panelClosedClass = ClassName "steps__panel--closed" :: ClassName
detailsClass = ClassName "steps__details" :: ClassName
emClass = ClassName "steps__emphasis" :: ClassName
codeClass = ClassName "steps__code" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      star & byClass headingClass ? do
        margin nil nil nil nil
      star & byClass triggerClass ? do
        border solid nil black
        margin nil nil nil nil
        display flex
        alignItems center
        width $ pct 100.0
        uncurry fontFamily Theme.montserrat
        fontSize $ rem 1.0
        textTransform uppercase
        lineHeight $ unitless 1.0
        backgroundColor black
        color white
        padding (em 0.75) (em 0.75) (em 0.75) (em 0.75)
        outlineWidth nil
      (star & byClass triggerClass) & pseudo "focus" ? do
        boxShadow $ singleton $ bsColor Theme.gold $ bsInset $ shadowWithSpread nil nil nil $ px 1.0
      (star & byClass triggerClass) & hover ? do
        color Theme.gold
      ((star & byClass triggerClass) |> star) |+ star ? do
        display inlineBlock
        marginLeft $ em 0.75
      star & byClass indicatorClass ? do
        transitionProperty "transform"
        transitionDuration "250ms"
      star & byClass indicatorOpenClass ? do
        transform $ rotate $ deg 90.0
      star & byClass panelClass ? do
        overflow Overflow.hidden
        backgroundColor Theme.darkerGray
        color white
        transitionProperty "height"
        transitionDuration "250ms"
        uncurry fontFamily Theme.roboto
        lineHeight $ unitless 1.5
        boxShadow $ singleton $ bsInset $ bsColor (rgba 0 0 0 0.5) $ shadowWithBlur nil (em 0.125) (em 0.3125)
        element "a" ? do
          color Theme.gold
          outlineColor Theme.gold
          outlineWidth nil
          outlineOffset $ em 0.125
          outlineStyle solid
        element "a" & pseudo "focus" ? do
          outlineWidth $ px 1.0
      star & byClass panelClosedClass ? do
        star & byClass detailsClass ? do
          opacity 0.0
          visibility hidden
          transform $ translateY $ em (-1.5)
      star & byClass detailsClass ? do
        let
          paddingX = em 1.5 @-@ px 1.0
          paddingY = em 0.75 @-@ px 1.0
        for_ [paddingRight, paddingLeft] (applyFlipped paddingX)
        for_ [paddingTop, paddingBottom] (applyFlipped paddingY)
        transitionProperties ["opacity", "visibility", "transform"]
        transitionDuration "250ms"
        element "p" ? do
          margin nil nil nil nil
        element "p" |+ element "p" ? do
          marginTop $ em 0.75
      star & byClass emClass ? do
        fontStyle normal
        color Theme.gold
      star & byClass codeClass ? do
        uncurry fontFamily Theme.inconsolata

useSteps :: forall m h p. MonadEffect m => Hook m (UseState (Maybe Int) <> UseAccordion Int <> h) (HH.HTML p (HookM m Unit))
useSteps = Hooks.do
  selection /\ selectionId <- useState $ Just 0
  accordion <- useAccordion
    (Accordion.defaultOptions Accordion.Single)
      { value = Just selection
      , onValueChange = Just $ Hooks.put selectionId
      , renderTrigger = renderTrigger
      , renderPanel = renderPanel
      , renderHeading = renderHeading
      }
    steps
  Hooks.pure $ HH.div_ [ accordion ]
  where

  renderHeading props = HH.h3 $ HP.class_ headingClass : props

  renderTrigger open props content =
    HH.button
      (HP.class_ triggerClass : props)
      [ HH.span
          [ HP.classes $
              catMaybes
                [ pure indicatorClass
                , if open then pure indicatorOpenClass else Nothing
                ]
          ]
          [ HSE.svg
              [ HSA.viewBox 0.0 0.0 12.0 12.0
              , HC.style do
                  width $ em 0.75
                  height $ em 0.75
              ]
              [ HSE.path
                  [ HP.style "fill: currentColor"
                  , HSA.d
                      [ m Abs 4.0 0.0
                      , l Abs 2.59 1.41
                      , l Abs 7.17 6.0
                      , l Rel (-4.58) 4.59
                      , l Abs 4.0 12.0
                      , l Rel 6.0 (-6.0)
                      ]
                  ]
              ]
          ]
      , HH.span_ content
      ]

  renderPanel { open, targetHeight } props content =
    HH.div
      ( [ HP.classes $ catMaybes [pure panelClass, find (not <<< const open) $ Just panelClosedClass]
        , HC.style $ fromMaybe (pure unit) $ height <<< px <$> targetHeight
        ]
        <> props
      )
      content

  steps =
    [ Tuple (HH.text "Step 1") $
        HH.div_
          [ HH.text "Set up your website on "
          , HH.a
            [ HP.href "https://pages.github.com/"
            , HP.target "_blank"
            ]
            [ HH.text "GitHub Pages" ]
          , HH.text ". If you're short on time, the "
          , HH.a
            [ HP.href "https://docs.github.com/en/pages/quickstart"
            , HP.target "_blank"
            ]
            [HH.text "Quickstart"]
          , HH.text """
              can help you to get started authoring markdown content in your
              browser and publishing it to your site in a few quick steps.
            """
          ]
    , Tuple (HH.text "Step 2") $ HH.div_
        [ HH.text "Choose the "
        , HH.em [HP.class_ emClass] [HH.text "purescri.pt"]
        , HH.text
            """
              subdomain that matches your repository or user/organization name,
              omitting the
            """
        , HH.em [ HP.class_ emClass ] [ HH.text "purescript- " ]
        , HH.text "prefix (if applicable). For example: "
        , HH.ul_
          [ HH.li_
              [ HH.text "User "
              , HH.em [HP.class_ emClass] [HH.text "bdover "]
              , HH.text "should use "
              , HH.em [HP.class_ emClass] [HH.text "bdover.purescri.pt "]
              , HH.text "."
              ]
          , HH.li_
              [ HH.text "Organization "
              , HH.em [HP.class_ emClass] [HH.text "purescript-contrib "]
              , HH.text "should use "
              , HH.em [HP.class_ emClass] [HH.text "contrib.purescri.pt "]
              , HH.text "."
              ]
          , HH.li_
              [ HH.text "Repository "
              , HH.em [HP.class_ emClass] [HH.text "purescript-css "]
              , HH.text "should use "
              , HH.em [HP.class_ emClass] [HH.text "css.purescri.pt "]
              , HH.text "."
              ]
          ]
        ]
    , Tuple (HH.text "Step 3") $
        HH.div_
          [ HH.p_
            [ HH.text "Add a file called "
            , HH.em [HP.classes [emClass, codeClass]] [HH.text "CNAME"]
            , HH.text " alongside your website content, usually located in a "
            , HH.em [HP.classes [emClass, codeClass]] [HH.text "gh-pages"]
            , HH.text " branch and/or under the "
            , HH.em [HP.classes [emClass, codeClass]] [HH.text "/docs"]
            , HH.text " directory."
            ]
          , HH.p_
            [ HH.text "The "
            , HH.em [HP.classes [emClass, codeClass]] [HH.text "CNAME"]
            , HH.text
                """
                  file should contain a single line consisting of your
                  subdomain (including the
                """
            , HH.em [HP.classes [emClass]] [HH.text ".purescri.pt"]
            , HH.text " part)."
            ]
          ]
    , Tuple (HH.text "Step 4") $ HH.text
        """
          Step 4 instructions coming soon
        """
    ]
      # mapWithIndex \i (summary /\ details) -> i /\ summary /\ HH.div [ HP.class_ detailsClass ] [ details ]
