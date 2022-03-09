module Domains.Site.Steps where

import Prelude

import CSS (StyleM, alignItems, backgroundColor, black, border, boxShadow, color, deg, display, element, em, flex, fontFamily, fontSize, height, hover, inlineBlock, lineHeight, margin, marginLeft, nil, opacity, outlineColor, outlineOffset, outlineStyle, outlineWidth, padding, paddingBottom, paddingLeft, paddingRight, paddingTop, pct, pseudo, px, rem, rgba, rotate, solid, star, textTransform, transform, transitionDuration, transitionProperties, transitionProperty, white, width, (&), (?), (@-@), (|+), (|>))
import CSS as CSS
import CSS.Box (bsColor, bsInset, shadowWithBlur, shadowWithSpread)
import CSS.Common (center, hidden)
import CSS.Display (visibility)
import CSS.Overflow (overflow)
import CSS.Overflow as Overflow
import CSS.Size (unitless)
import CSS.Text.Transform (uppercase)
import CSS.Transform (translateY)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Array (catMaybes, mapWithIndex, (:))
import Data.Foldable (find)
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (singleton)
import Data.Traversable (for_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Domains.Site.Markdown (UseMarkdown, useMarkdown)
import Domains.Site.Theme as Theme
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
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
import MarkdownIt (MarkdownIt)

step1Content = """
Set up your website on [GitHub Pages](https://pages.github.com/). If you're
short on time, the [Quickstart](https://docs.github.com/en/pages/quickstart) can
help you to get started authoring markdown content in your browser and
publishing it to your site in a few quick steps.
""" :: String

step2Content = """
Choose the _purescri.pt_ subdomain that matches your repository or
user/organization name, omitting the _purescript-_ prefix (if applicable). For
example:

* User _jhopper_ should use _jhopper.purescri.pt_.
* Organization _purescript-contrib_ should use _contrib.purescri.pt_.
* Repository _purescript-css_ should use _css.purescri.pt_.
""" :: String

step3Content = """
Add a file called _`CNAME`_ alongside your website content, usually located in a
_`gh-pages`_ branch and/or under the _`/docs`_ directory.

The _`CNAME`_ file should contain a single line consisting of your subdomain
(including the _`.purescri.pt`_ part).
""" :: String

step4Content = """
Now it's time to register your domain by adding it to
[this list](https://github.com/purescript-domains/dns/edit/main/domains.yml).
You can edit the file right in your browser and then follow the options at the
bottom of the form to submit a pull request.

Please keep an eye on your pull request in case we have any questions.
Otherwise, your registration will be processed immediately once merged. Keep in
mind that DNS changes may take up to 24 hours to propagate.
""" :: String

headingClass = ClassName "steps__heading" :: ClassName
triggerClass = ClassName "steps__trigger" :: ClassName
indicatorClass = ClassName "steps__indicator" :: ClassName
indicatorOpenClass = ClassName "steps__indicator--open" :: ClassName
panelClass = ClassName "steps__panel" :: ClassName
panelClosedClass = ClassName "steps__panel--closed" :: ClassName
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
          paddingX = em 1.75 @-@ px 1.0
          paddingY = em 1.25 @-@ px 1.0
        for_ [paddingRight, paddingLeft] (applyFlipped paddingX)
        for_ [paddingTop, paddingBottom] (applyFlipped paddingY)
        transitionProperties ["opacity", "visibility", "transform"]
        transitionDuration "250ms"

useSteps
  :: forall r m h p
   . MonadAsk { markdownIt :: MarkdownIt, markdownRef :: Ref Int | r } m
  => MonadEffect m
  => Hook
       m
       (UseMarkdown <> UseMarkdown <> UseMarkdown <> UseMarkdown <> UseState (Maybe Int) <> UseAccordion Int <> h)
       (HH.HTML p (HookM m Unit))
useSteps = Hooks.do

  step1Markup <- useMarkdown step1Content
  step2Markup <- useMarkdown step2Content
  step3Markup <- useMarkdown step3Content
  step4Markup <- useMarkdown step4Content

  let
    steps =
      [ "Step 1" /\ step1Markup
      , "Step 2" /\ step2Markup
      , "Step 3" /\ step3Markup
      , "Step 4" /\ step4Markup
      ]
        # mapWithIndex
            \i (summary /\ content) ->
              i /\ HH.text summary
                /\ HH.div
                     [ HP.class_ detailsClass ]
                     [ content ]

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
