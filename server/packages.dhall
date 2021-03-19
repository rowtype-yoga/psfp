{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , "package-name" =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ],
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
    --   https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200309/packages.dhall sha256:9221987b4e7ea99ccd0efbe056f7bebc872cd92e0058efe5baa181d73359e7b3
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210317/packages.dhall sha256:e2e744972f9b60188dcf07f41418661b505c9ee2e9f91e57e67daefad3a5ae09

let overrides =
      { react-basic-hooks =
              upstream.react-basic-hooks
          //  { repo =
                  "https://github.com/i-am-the-slime/purescript-react-basic-hooks.git"
              , version = "e04b106ab2dfda3f9a1407420c434a908ff72b90"
              }
      , css =
              upstream.css
          //  { repo = "https://github.com/i-am-the-slime/purescript-css.git"
              , version = "8ea0bab17c268d9c62a09892d7ba231dcbe6308b"
              }

      , node-net = upstream.node-net
        // { repo = "https://github.com/i-am-the-slime/purescript-node-net.git"
        , version = "b4efc12a6bc8df695f90e408290b3b82dcd8548b"
        }
      }

let additions =
      { react-testing-library =
        { dependencies =
          [ "aff-promise"
          , "console"
          , "debug"
          , "effect"
          , "foreign"
          , "foreign-object"
          , "psci-support"
          , "react-basic-hooks"
          , "remotedata"
          , "run"
          , "simple-json"
          , "spec"
          , "spec-discovery"
          ]
        , repo =
            "https://github.com/i-am-the-slime/purescript-react-testing-library.git"
        , version = "13a63056506a3ce32572e326130be325931ba7c0"
        }
      , pseudo-random =
        { dependencies =
          [ "prelude", "console", "effect", "lcg", "arrays", "st" ]
        , repo = "https://github.com/opyapeus/purescript-pseudo-random.git"
        , version = "7715e8a2c096c480a093a5e0a6df1ece4df5ed2a"
        }
      , oneof =
        { dependencies =
          [ "assert"
          , "console"
          , "effect"
          , "foreign"
          , "foreign-object"
          , "literal"
          , "maybe"
          , "newtype"
          , "proxy"
          , "psci-support"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/jvliwanag/purescript-oneof.git"
        , version = "0325fddf6ee8a181fac2128c9b542c2c01ddd361"
        }
      , literal =
        { dependencies =
          [ "assert"
          , "effect"
          , "console"
          , "integers"
          , "numbers"
          , "partial"
          , "psci-support"
          , "unsafe-coerce"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/jvliwanag/purescript-literal.git"
        , version = "7b2ae20f77c67b7e419a92fdd0dc7a09b447b18e"
        }
      , justifill =
        { dependencies = [ "record", "typelevel-prelude" ]
        , repo = "https://github.com/i-am-the-slime/purescript-justifill.git"
        , version = "2de06260ae8e37355678198180bbdd06c91457e3"
        }
      , matryoshka =
        { dependencies =
          [ "prelude", "fixed-points", "free", "transformers", "profunctor" ]
        , repo = "https://github.com/slamdata/purescript-matryoshka.git"
        , version = "caaca2d836d52159ba7963333996286a00428394"
        }
      , interpolate =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/jordanmartinez/purescript-interpolate.git"
        , version = "v2.0.1"
        }
      , yoga-components = ../components/spago.dhall as Location
      , foreign-generic = {
        dependencies =
        [ "effect"
        , "foreign"
        , "foreign-object"
        , "ordered-collections"
        , "exceptions"
        , "record"
        , "identity"
        ]
        , repo =
            "https://github.com/fsoikin/purescript-foreign-generic.git"
        , version =
            "c9ceaa48d4a03ee3db55f1abfb45f830cae329e7"
        }
      , uuid = {
        dependencies =
        [ "console"
        , "effect"
        , "psci-support"
        , "spec"
        , "foreign-generic"
        ]
        , repo =
            "https://github.com/spicydonuts/purescript-uuid.git"
        , version =
            "7c9b1a1261aadb4db4886b3123683ca29c2663a5"
        }
      , express = {
        dependencies =
        [ "aff"
        , "console"
        , "effect"
        , "foreign"
        , "foreign-generic"
        , "node-http"
        , "psci-support"
        , "test-unit"
        ]
        , repo =
            "https://github.com/i-am-the-slime/purescript-express.git"
        , version =
            "0.14"
        }
    , simple-json = {
        dependencies =
        [ "prelude"
        , "typelevel-prelude"
        , "record"
        , "variant"
        , "nullable"
        , "foreign-object"
        , "foreign"
        , "exceptions"
        , "arrays"
        ]
        , repo = "https://github.com/angelinatarapko/purescript-simple-json.git"
        , version = "patch-1"
    }
}

in  upstream // overrides // additions