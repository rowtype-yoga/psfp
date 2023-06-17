{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "express"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "httpurple"
  , "httpurple-yoga-json"
  , "integers"
  , "js-date"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-http"
  , "node-net"
  , "node-process"
  , "ordered-collections"
  , "parallel"
  , "posix-types"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing-duplex"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "uuid"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
