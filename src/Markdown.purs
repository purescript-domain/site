module Domains.Site.Markdown where

import Prelude

import Control.Monad.Reader.Class (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (RefLabel(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>), Hook, UseEffect, useTickEffect)
import Halogen.Hooks as Hooks
import MarkdownIt (MarkdownIt)
import MarkdownIt as MD
import Nonbili.DOM (setInnerHTML)
import Web.HTML.HTMLElement as HTMLElement

useMarkdown
  :: forall m r p i h
   . MonadEffect m
  => MonadAsk { markdownIt :: MarkdownIt | r } m
  => String
  -> Hook m (UseEffect <> h) (HH.HTML p i)
useMarkdown md = Hooks.do
  let containerRef = RefLabel "markdownOutput"
  Hooks.captures
    { md }
    useTickEffect do
      markdownIt <- asks _.markdownIt
      markup <- liftEffect $ MD.render markdownIt md
      container <- Hooks.getHTMLElementRef containerRef
      traverse_
        (liftEffect <<< flip setInnerHTML markup <<< HTMLElement.toElement)
        container
      pure Nothing
  Hooks.pure $ HH.div [HP.ref containerRef] []
