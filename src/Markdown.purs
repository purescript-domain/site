module Domains.Site.Markdown where

import Prelude

import CSS (StyleM, color, element, em, fontFamily, fontStyle, lineHeight, margin, marginTop, nil, star, (&), (?), (|>), (|+))
import CSS as CSS
import CSS.Common (normal)
import CSS.Size (unitless)
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Domains.Site.Theme as Theme
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (ClassName(..), RefLabel(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, Pure, UseEffect, UseState, useLifecycleEffect, useState, useTickEffect)
import Halogen.Hooks as Hooks
import MarkdownIt (MarkdownIt)
import MarkdownIt as MD
import Nonbili.DOM (setInnerHTML)
import Web.HTML.HTMLElement as HTMLElement

rootClass = ClassName "prose" :: ClassName

css :: StyleM Unit
css =
  let
    byClass (ClassName c) = CSS.byClass c
  in
    do
      star & byClass rootClass ? do
        uncurry fontFamily Theme.roboto
        lineHeight $ unitless 1.5
        element "em" ? do
          fontStyle normal
          color Theme.gold
        element "code" ? do
          uncurry fontFamily Theme.inconsolata
        element "a" ? do
          color Theme.gold
      (star & byClass rootClass) |> star ? do
        margin nil nil nil nil
      ((star & byClass rootClass) |> star) |+ star ? do
        marginTop $ em 0.75

foreign import data UseMarkdown :: HookType

instance HookNewtype UseMarkdown (UseState (Maybe String) <> UseEffect <> UseEffect <> Pure)

useMarkdown
  :: forall m r p i
   . MonadEffect m
  => MonadAsk { markdownIt :: MarkdownIt, markdownRef :: Ref Int | r } m
  => String
  -> Hook m UseMarkdown (HH.HTML p i)
useMarkdown md =
  Hooks.wrap $
    Hooks.do
      containerRefs /\ containerRefId <- useState Nothing

      let containerRef = RefLabel <$> containerRefs

      useLifecycleEffect do
        ref <- asks _.markdownRef
        refNum <- liftEffect $ Ref.modify (_ + 1) ref
        Hooks.put containerRefId $ Just $ "useMarkdown" <> show refNum
        pure Nothing

      Hooks.captures
        { md, containerRef }
        useTickEffect
        do
          case containerRef of
            Just containerRef' -> do
              markdownIt <- asks _.markdownIt
              markup <- liftEffect $ MD.render markdownIt md
              container <- Hooks.getHTMLElementRef containerRef'
              traverse_
                (liftEffect <<< flip setInnerHTML markup <<< HTMLElement.toElement)
                container
              pure Nothing
            Nothing ->
              pure Nothing

      Hooks.pure $
        case containerRef of
          Nothing ->
            HH.div_ []
          Just containerRef' ->
            HH.div [ HP.ref containerRef', HP.class_ rootClass ] []
