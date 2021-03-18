{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "avar"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "psci-support"
  , "stringutils"
  , "tuples"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
