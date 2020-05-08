module Yoga.Grimoire.Spell.Component where

import Prelude
import CSS (AlignItemsValue, JustifyContentValue, spaceBetween)
import CSS.Common (baseline)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import React.Basic (ReactComponent)
import React.Basic.DOM as R
import React.Basic.Helpers (classSpan, jsx)
import React.Basic.Hooks (Ref, component)
import React.Basic.Hooks as React
import Record.Extra (pick)
import Web.DOM (Node)
import Yoga.Box.Component as Box
import Yoga.Card.Component (mkCard)
import Yoga.Centre.Component as Centre
import Yoga.Cluster.Component as Cluster
import Yoga.Cover.Component as Cover
import Yoga.Grimoire.Spell.Styles (styles)
import Yoga.Grimoire.Spell.Styles as Style
import Yoga.Spell.Types (Spell)
import Yoga.Stack.Component as Stack
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)

type Props
  = { spell ∷ Spell
    , cardRef ∷ Maybe (Ref (Nullable Node))
    | Style.PropsR
    }

makeComponent ∷ Effect (ReactComponent Props)
makeComponent = do
  box <- Box.makeComponent
  centre <- Centre.makeComponent
  stack <- Stack.makeComponent
  cluster <- Cluster.makeComponent
  card <- mkCard
  cover <- Cover.makeComponent
  useStyles <- makeStylesJSS styles
  component "Spell" \(props@{ spell } ∷ Props) -> React.do
    style <- useStyles (pick props)
    theme ∷ CSSTheme <- useTheme
    let
      headerCluster =
        jsx cluster
          { space: "var(--s-5)"
          , align: baseline ∷ AlignItemsValue
          , justify: spaceBetween ∷ JustifyContentValue
          }
          [ R.div_
              [ classSpan style.name [ R.text spell.name ]
              , classSpan style.signature [ R.text spell.signature ]
              ]
          ]
    pure
      $ jsx card { divRef: props.cardRef, className: style.card }
          [ jsx box { className: style.container, padding: "var(--s-1)" }
              [ jsx stack { space: "var(--s-3)" }
                  [ headerCluster
                  , classSpan style.description [ R.text spell.description ]
                  ]
              ]
          ]
