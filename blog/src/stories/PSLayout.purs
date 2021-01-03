module Stories.PSLayout where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import MDX (MDX, renderWithReact)
import MDXProvider (MdxProviderProps, mkLiveMdxProviderComponent)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import PSLayout (mkLayout)
import React.Basic (JSX, ReactComponent, fragment)
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
# Monad

A<Sc>ccording to [Hippolytus](https://en.wikipedia.org/wiki/Hippolytus_of_Rome "Hippolytus of Rome")</Sc>, the worldview was inspired by the [Pythagoreans](https://en.wikipedia.org/wiki/Pythagoreanism "Pythagoreanism"), who called the first thing that came into existence the "monad", which begat (bore) the [dyad](https://en.wikipedia.org/wiki/Dyad_(Greek_philosophy) "Dyad (Greek philosophy)") (from the Greek word for two), which begat the [numbers](https://en.wikipedia.org/wiki/Number "Number"), which begat the [point](https://en.wikipedia.org/wiki/Point_(geometry) "Point (geometry)"), begetting [lines](https://en.wikipedia.org/wiki/Line_(geometry) "Line (geometry)") or [finiteness](https://en.wiktionary.org/wiki/finite "wikt:finite"), etc.<sup>[[2]](https://en.wikipedia.org/wiki/Monad_(philosophy)#cite_note-2)</sup> It meant [divinity](https://en.wikipedia.org/wiki/Divinity "Divinity"), the first being, or the totality of all beings, referring in [cosmogony](https://en.wikipedia.org/wiki/Cosmogony "Cosmogony") (creation theories) variously to source acting alone and/or an indivisible origin and [equivalent comparators](https://en.wikipedia.org/wiki/Abstraction_(philosophy) "Abstraction (philosophy)").^[[3]](https://en.wikipedia.org/wiki/Monad_(philosophy)#cite_note-3)

no way is out

```purescript
module You.Are.An where

asshole = "You"
```

```purescript
--result 12
module Bla where
--start here
pigboy = "Piggy {-hi-} Pigster"
--end here
```

```javascript
'use strict'
undefined = NaN || null
undefined = NaN || null
undefined = NaN || null
undefined = NaN || null
undefined = NaN || null
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
          mdxCode <- compileMDX example
          liftEffect $ setMdx mdxCode
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
