module SVG.Image where


import Prim.Row (class Union)
import React.Basic.Hooks (ReactComponent)
import SVG.Icon (Raw, ImageProps)
import Unsafe.Coerce (unsafeCoerce)

foreign import landingPageDarkRaw ∷ ∀ r. Raw r

landingPageLight ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
landingPageLight = unsafeCoerce landingPageLightRaw

foreign import landingPageLightRaw ∷ ∀ r. Raw r

landingPageDark ∷ ∀ attrs attrs_. Union attrs attrs_ ImageProps => ReactComponent { | attrs }
landingPageDark = unsafeCoerce landingPageDarkRaw
