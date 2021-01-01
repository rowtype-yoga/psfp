module MDX where

import Prelude
import Control.Promise (Promise, toAff)
import Effect.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)

type MDXProps =
  { mdxType ∷ String
  , children ∷ MDXChildren
  }

newtype MDX = MDX
  { props ∷ MDXProps
  }

foreign import data MDXChildren ∷ Type

childrenProps ∷ MDXChildren -> MDXProps
childrenProps = unsafeCoerce >>> _.props

foreign import mdxImpl ∷ String -> Promise MDX

mdx ∷ String -> Aff MDX
mdx = toAff <<< mdxImpl

foreign import renderWithReact ∷ String -> Array MDX
