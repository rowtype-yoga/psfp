module Stories.PSLayout where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MDX (MDX(..), renderWithReact)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import PSLayout (MdxProviderProps)
import PSLayout (mkLayout, mkLiveMdxProviderComponent)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.DOM as R
import React.Basic.Hooks (reactChildrenFromArray, reactComponent, reactComponentWithChildren)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Unsafe.Coerce (unsafeCoerce)

default ∷
  { title ∷ String
  }
default =
  { title: "Page/Landing"
  }

fetch ∷ M.Fetch
fetch = M.fetch windowFetch

compileMDX ∷ String -> Aff (Array MDX)
compileMDX body = do
  res <- fetch (M.URL "/mdx") { method: M.postMethod, body }
  txt <- M.text res
  pure $ renderWithReact txt

example ∷ String
example =
  """
# Help me!
I am stuck in this shit
no way is out

```purescript
module You.Are.An where

asshole = "You"
```

This is particularly lovely.
I cannot recommend this party enough.

## Nobody has ever seen this problem in the flesh
"""

landingPage ∷ Effect JSX
landingPage = do
  mdxProvider <- mkLiveMdxProviderComponent windowFetch
  fakeMdxProvider <- mkFakeMdxProvider
  layout <- mkLayout windowFetch
  let
    mkCompo =
      reactComponent "Landing Page Story" \{} -> React.do
        mdx /\ setMdx <- React.useState' []
        useAff unit do
          mdx <- compileMDX example
          liftEffect $ setMdx mdx
        pure
          $ React.element layout
              { children:
                reactChildrenFromArray mdx
              , mdxProviderComponent: mdxProvider
              , siteInfo:
                { siteMetadata:
                  { menuLinks: []
                  , title: "Page"
                  }
                }
              }
  compo <- mkCompo
  pure $ React.element compo {}
  where
    mkFakeMdxProvider ∷ Effect (ReactComponent MdxProviderProps)
    mkFakeMdxProvider =
      reactComponentWithChildren "Fake MDX Provider" \{ children } -> React.do
        pure
          $ fragment (unsafeCoerce children)
