{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "avar"
  , "console"
  , "debug"
  , "effect"
  , "express"
  , "functions"
  , "maybe"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-net"
  , "node-process"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "stringutils"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
