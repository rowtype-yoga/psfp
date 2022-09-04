{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "distributive"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "functions"
  , "identity"
  , "integers"
  , "invariant"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "nullable"
  , "ordered-collections"
  , "orders"
  , "prelude"
  , "psci-support"
  , "strings"
  , "stringutils"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
