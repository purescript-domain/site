module Domains.Site.Main where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Data.Maybe (Maybe(..))
import Domains.Site.App as App
import Domains.Site.Route as Route
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import MarkdownIt (newMarkdownIt)
import MarkdownIt as MD
import Routing.Hash (matchesWith)

main :: Effect Unit
main = do
  markdownIt <- newMarkdownIt MD.Default mempty
  markdownRef <- Ref.new 0
  { emitter, listener } <- HS.create
  runHalogenAff do
    body <- awaitBody
    let app = H.hoist (flip runReaderT { markdownIt, markdownRef }) App.component
    io <- runUI app unit body
    liftEffect do
      void $ HS.subscribe emitter $ runHalogenAff <<< io.query <<< H.mkTell <<< App.Navigate
      void $ matchesWith (Just <<< Route.parse) $ const $ HS.notify listener
