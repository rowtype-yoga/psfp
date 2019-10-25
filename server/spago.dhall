{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "express"
    , "maybe"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-process"
    , "psci-support"
    , "simple-json"
    , "stringutils"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
