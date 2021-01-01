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
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201223/packages.dhall sha256:a1a8b096175f841c4fef64c9b605fb0d691229241fd2233f6cf46e213de8a185

let overrides =
      { css =
              upstream.css
          //  { repo = "https://github.com/i-am-the-slime/purescript-css.git"
              , version = "8ea0bab17c268d9c62a09892d7ba231dcbe6308b"
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
        , version = "master"
        }
      , pseudo-random =
        { dependencies =
          [ "prelude", "console", "effect", "lcg", "arrays", "st" ]
        , repo = "https://github.com/opyapeus/purescript-pseudo-random.git"
        , version = "7715e8a2c096c480a093a5e0a6df1ece4df5ed2a"
        }
      , justifill =
        { dependencies = [ "record", "typelevel-prelude" ]
        , repo = "https://github.com/i-am-the-slime/purescript-justifill.git"
        , version = "master"
        }
      , matryoshka =
        { dependencies =
          [ "prelude", "fixed-points", "free", "transformers", "profunctor" ]
        , repo = "https://github.com/slamdata/purescript-matryoshka.git"
        , version = "caaca2d836d52159ba7963333996286a00428394"
        }
      , yoga-components = ../components/spago.dhall as Location
      , ry-blocks = ../../ry-blocks/spago.dhall as Location
      }

in  upstream // overrides // additions
