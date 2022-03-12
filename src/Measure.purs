module Domains.Site.Measure where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..), curry, fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (genUUID)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen (RefLabel(..))
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, Pure, UseEffect, UseState, useLifecycleEffect, useState, useTickEffect)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Web.HTML.HTMLElement (DOMRect)
import Web.HTML.HTMLElement as HTMLElement
import Web.ResizeObserver (resizeObserver)
import Web.ResizeObserver as ResizeObserver

foreign import data UseMeasure :: HookType

instance HookNewtype UseMeasure (UseRefLabel <> UseState DOMRect <> UseEffect <> Pure)

useMeasure :: forall m. MonadEffect m => Hook m UseMeasure (Maybe (RefLabel /\ DOMRect))
useMeasure =
  Hooks.wrap $
    Hooks.do
      refLabel <- useRefLabel
      rect /\ rectId <- useState { top: 0.0, right: 0.0, bottom: 0.0, left: 0.0, width: 0.0, height: 0.0 }
      Hooks.captures { refLabel }
        useTickEffect
        do
          { emitter, listener } <- liftEffect HS.create
          obs <- liftEffect
            $ resizeObserver
            $ curry
            $ fst >>> traverse_
                \{ contentRect: { top, right, bottom, left, width, height } } ->
                  HS.notify listener $ Hooks.put rectId { top, right, bottom, left, width, height }
          for_ refLabel $ Hooks.getHTMLElementRef >=> \el -> do
            let
              l = case refLabel of
                Nothing -> "Nothing"
                Just (RefLabel x) -> x
            liftEffect $ Console.logShow $ l /\ isJust el
            traverse_ (liftEffect <<< flip (flip ResizeObserver.observe {}) obs <<< HTMLElement.toElement) el
          subscription <- Hooks.subscribe emitter
          pure $ Just do
            Hooks.unsubscribe subscription
            liftEffect $ ResizeObserver.disconnect obs
      Hooks.pure $ (_ /\ rect) <$> refLabel

foreign import data UseRefLabel :: HookType

instance HookNewtype UseRefLabel (UseState (Maybe String) <> UseEffect <> Pure)

useRefLabel :: forall m. MonadEffect m => Hook m UseRefLabel (Maybe RefLabel)
useRefLabel =
  Hooks.wrap $
    Hooks.do
      uuid /\ uuidId <- useState Nothing
      useLifecycleEffect do
        uuid' <- liftEffect genUUID
        Hooks.put uuidId $ Just $ UUID.toString uuid'
        pure Nothing
      Hooks.pure $ RefLabel <$> uuid
