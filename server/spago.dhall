{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "avar"
    , "console"
    , "debug"
    , "effect"
    , "express"
    , "maybe"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-process"
    , "psci-support"
    , "simple-json"
    , "spec"
    , "spec-discovery"
    , "stringutils"
    , "uuid"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
