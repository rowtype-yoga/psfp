{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff"
    , "avar"
    , "console"
    , "debug"
    , "effect"
    , "foldable-traversable"
    , "maybe"
    , "newtype"
    , "ordered-collections"
    , "psci-support"
    , "record-extra"
    , "simple-json"
    , "stringutils"
    , "tuples"
    , "validation"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
