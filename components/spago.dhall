{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "css"
  , "debug"
  , "effect"
  , "interpolate"
  , "justifill"
  , "matryoshka"
  , "milkis"
  , "oneof"
  , "psci-support"
  , "pseudo-random"
  , "react-basic"
  , "react-basic-hooks"
  , "react-testing-library"
  , "record-extra"
  , "refs"
  , "spec-discovery"
  , "string-parsers"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
