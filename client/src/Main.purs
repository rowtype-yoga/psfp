module Example (main) where

import Prelude
import Container.Component (mkContainer)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import React.Basic (JSX, element)
import Theme (fromTheme)
import Theme.Default (darkTheme)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

main ∷ Effect Unit
main = do
  container <- mkContainer
  let
    appEl = element container { theme: fromTheme darkTheme, children: [] }
  if isServerSide then
    void (log (renderToString appEl))
  else
    void do
      window <- DOM.window
      document <- DOM.document window
      let
        node = DOM.toNonElementParentNode document
      elem <- DOM.getElementById "app" node
      let
        element' = unsafePartial (fromJust elem)
      root <- createRoot element'
      renderRoot appEl root

--   ReactDOM.render appEl element'
foreign import isServerSide ∷ Boolean

foreign import renderToString ∷ JSX -> String

foreign import data ReactRoot ∷ Type

foreign import createRoot ∷ Element -> Effect ReactRoot

foreign import renderRoot ∷ JSX -> ReactRoot -> Effect Unit
