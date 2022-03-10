let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220216/packages.dhall sha256:890466a5e3ed4793ee702d8df8ef85a025fbacbdfeb63c73597aef2795c06845

in  upstream
  with css.repo = "https://github.com/purescript-contrib/purescript-css.git"
  with css.version = "main"
  with halogen-headless =
    { dependencies =
      [ "arrays"
      , "dom-indexed"
      , "effect"
      , "foldable-traversable"
      , "halogen"
      , "halogen-hooks"
      , "halogen-subscriptions"
      , "maybe"
      , "prelude"
      , "record"
      , "strings"
      , "tuples"
      , "typelevel-prelude"
      , "unfoldable"
      , "web-dom"
      , "web-events"
      , "web-html"
      , "web-resize-observer"
      , "web-uievents"
      ]
    , repo = "https://github.com/nsaunders/purescript-halogen-headless.git"
    , version = "master"
    }
  with markdown-it =
    { dependencies = ["effect", "prelude", "options"]
    , repo = "https://github.com/nonbili/purescript-markdown-it.git"
    , version = "v0.5.0"
    }
  with nonbili-dom =
    { dependencies = ["effect", "web-dom", "web-html"]
    , repo = "https://github.com/nonbili/purescript-nonbili-dom.git"
    , version = "v0.3.0"
    }
