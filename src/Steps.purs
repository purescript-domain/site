module PurescriPT.Site.Steps where

import Prelude

import CSS (StyleM, alignItems, backgroundColor, black, border, color, deg, display, em, flex, fontFamily, fontSize, height, hover, inlineBlock, lineHeight, margin, marginLeft, nil, outlineColor, outlineStyle, outlineWidth, padding, pct, pseudo, px, rem, rotate, solid, star, textTransform, transform, transitionDuration, transitionProperty, white, width, (&), (?), (|+), (|>))
import CSS as CSS
import CSS.Common (center)
import CSS.Overflow (hidden, overflow)
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import Data.Array (catMaybes, mapWithIndex, (:))
import Data.Maybe (Maybe(..), fromMaybe)
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
import PurescriPT.Site.Theme as Theme

headingClass = ClassName "steps__heading" :: ClassName
triggerClass = ClassName "steps__trigger" :: ClassName
indicatorClass = ClassName "steps__indicator" :: ClassName
indicatorOpenClass = ClassName "steps__indicator--open" :: ClassName
panelClass = ClassName "steps__panel" :: ClassName
detailsClass = ClassName "steps__details" :: ClassName

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
        uncurry fontFamily Theme.logoFont
        fontSize $ rem 1.0
        textTransform uppercase
        lineHeight $ unitless 1.0
        backgroundColor black
        color white
        padding (em 0.75) (em 0.75) (em 0.75) (em 0.75)
        outlineStyle solid
        outlineColor Theme.gold
        outlineWidth nil
      (star & byClass triggerClass) & pseudo "focus" ? do
        outlineWidth $ px 1.0
      (star & byClass triggerClass) & hover ? do
        color Theme.gold
      ((star & byClass triggerClass) |> star) |+ star ? do
        display inlineBlock
        marginLeft $ em 0.75
      star & byClass indicatorOpenClass ? do
        transitionProperty "transform"
        transitionDuration "250ms"
      star & byClass indicatorOpenClass ? do
        transform $ rotate $ deg 90.0
      star & byClass panelClass ? do
        overflow hidden
        transitionProperty "height"
        transitionDuration "250ms"
      star & byClass detailsClass ? do
        padding (em 0.75) (em 0.75) (em 0.75) (em 0.75)

useSteps :: forall m h p. MonadEffect m => Hook m (UseState (Maybe Int) <> UseAccordion Int <> h) (HH.HTML p (HookM m Unit))
useSteps = Hooks.do
  selection /\ selectionId <- useState $ Just 0
  accordion <- useAccordion
    (Accordion.defaultOptions Accordion.Single)
      { value = Just selection
      , onValueChange = Just $ Hooks.put selectionId
      , renderTrigger = renderStepTrigger
      , renderPanel = renderStepPanel
      , renderHeading = renderStepHeading
      }
    steps
  Hooks.pure $ HH.div_ [ accordion ]
  where

  renderStepHeading props = HH.h3 $ HP.class_ headingClass : props

  renderStepTrigger open props content =
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

  renderStepPanel { targetHeight } props content =
    HH.div
      ([ HP.class_ panelClass, HC.style $ fromMaybe (pure unit) $ height <<< px <$> targetHeight ] <> props)
      content

  steps =
    [ Tuple (HH.text "Step 1") $ HH.text
        """
          Step 1 instructions coming soon
        """
    , Tuple (HH.text "Step 2") $ HH.text
        """
          Step 2 instructions coming soon
        """
    , Tuple (HH.text "Step 3") $ HH.text
        """
          Step 3 instructions coming soon
        """
    , Tuple (HH.text "Step 4") $ HH.text
        """
          Step 4 instructions coming soon
        """
    ]
      # mapWithIndex \i (summary /\ details) -> i /\ summary /\ HH.div [ HP.class_ detailsClass ] [ details ]
