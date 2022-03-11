module Domains.Site.CSS where

import Prelude

import CSS (render, renderedSheet)
import Data.Array (elemIndex, (!!))
import Data.Either (either)
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import Effect.Class.Console (log, warn)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Async (writeTextFile)
import Node.Process (argv)
import Domains.Site.App as App
import Domains.Site.Home as Home
import Domains.Site.Markdown as Prose
import Domains.Site.Steps as Steps
import Domains.Site.SupportButton as SupportButton

main :: Effect Unit
main = do
  args <- argv
  let to = fromMaybe "./index.css" $ "--to" `elemIndex` args >>= \i -> args !! (i + 1)
  maybe
    (warn "Warning: No CSS output")
    ( \cssString ->
        writeTextFile UTF8 to cssString $
          either throwException \_ -> log "Successfully wrote CSS output."
    )
    $ renderedSheet
    $ render
    $ App.css <> Home.css <> Prose.css <> Steps.css <> SupportButton.css
