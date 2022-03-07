{ name = "domains-site"
, dependencies =
  [ "arrays"
  , "console"
  , "css"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "halogen-headless"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "nonempty"
  , "prelude"
  , "routing"
  , "routing-duplex"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
