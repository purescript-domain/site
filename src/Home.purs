module PurescriPT.Site.Home where

import Prelude

import CSS (StyleM, alignItems, backgroundColor, black, border, byClass, color, deg, display, em, flex, fontFamily, fontSize, height, hover, inlineBlock, letterSpacing, lineHeight, margin, marginLeft, nil, outline, outlineColor, outlineStyle, outlineWidth, padding, pct, pseudo, px, rem, rotate, solid, star, textTransform, transform, transitionDuration, transitionProperty, white, width, (&), (?), (|+), (|>))
import CSS as CSS
import CSS.Common (center)
import CSS.Overflow (hidden, overflow)
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import Data.Array (catMaybes, length, mapWithIndex, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.Headless.Accordion (useAccordion)
import Halogen.Headless.Accordion as Accordion
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Halogen.Svg.Attributes (CommandPositionReference(..), l, m)
import Halogen.Svg.Attributes as HSA
import Halogen.Svg.Elements as HSE
import PurescriPT.Site.Theme as Theme

stepHeadingClass = ClassName "home__step-heading" :: ClassName
stepTriggerClass = ClassName "home__step-trigger" :: ClassName
stepIndicatorClass = ClassName "home__step-indicator" :: ClassName
stepIndicatorOpenClass = ClassName "home__step-indicator--open" :: ClassName
stepPanelClass = ClassName "home__step-panel" :: ClassName
stepPanelContentClass = ClassName "home__step-panel-content" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      star & byClass stepHeadingClass ? do
        margin nil nil nil nil
      star & byClass stepTriggerClass ? do
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
      (star & byClass stepTriggerClass) & pseudo "focus" ? do
        outlineWidth $ px 1.0
      (star & byClass stepTriggerClass) & hover ? do
        color Theme.gold
      ((star & byClass stepTriggerClass) |> star) |+ star ? do
        display inlineBlock
        marginLeft $ em 0.75
      star & byClass stepIndicatorOpenClass ? do
        transitionProperty "transform"
        transitionDuration "250ms"
      star & byClass stepIndicatorOpenClass ? do
        transform $ rotate $ deg 90.0
      star & byClass stepPanelClass ? do
        overflow hidden
        transitionProperty "height"
        transitionDuration "250ms"
      star & byClass stepPanelContentClass ? do
        padding (em 0.75) (em 0.75) (em 0.75) (em 0.75)

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
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

  renderStepHeading props = HH.h3 $ HP.class_ stepHeadingClass : props

  renderStepTrigger open props content =
    HH.button
      (HP.class_ stepTriggerClass : props)
      [ HH.span
          [ HP.classes $
              catMaybes
                [ pure stepIndicatorClass
                , if open then pure stepIndicatorOpenClass else Nothing
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
      ([ HP.class_ stepPanelClass, HC.style $ fromMaybe (pure unit) $ height <<< px <$> targetHeight ] <> props)
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
      # mapWithIndex \i (summary /\ details) -> i /\ summary /\ HH.div [ HP.class_ stepPanelContentClass ] [ details ]
