{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "yoga-components"
, dependencies =
  [ "console"
  , "css"
  , "debug"
  , "effect"
  , "interpolate"
  , "justifill"
  , "matryoshka"
  , "milkis"
  , "psci-support"
  , "pseudo-random"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "react-testing-library"
  , "record-extra"
  , "refs"
  , "ry-blocks"
  , "simple-json"
  , "spec-discovery"
  , "string-parsers"
  , "untagged-union"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
