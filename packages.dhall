let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220216/packages.dhall sha256:890466a5e3ed4793ee702d8df8ef85a025fbacbdfeb63c73597aef2795c06845

in  upstream
  with css.repo = "https://github.com/purescript-contrib/purescript-css.git"
  with css.version = "main"
  with halogen-headless =
    { dependencies =
        ( https://raw.githubusercontent.com/nsaunders/purescript-halogen-headless/master/spago.dhall sha256:977fb4952630c4427a5592561a5aeab07db69b98ec6218e18cd0b277570ad005
        ).dependencies
    , repo = "https://github.com/nsaunders/purescript-halogen-headless.git"
    , version = "master"
    }
