module Decorator.FullScreen where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (element)
import Theme.Default (darkTheme)
import Theme.Provider (mkThemeProvider)

fullScreenDecorator ∷ JSX -> Effect JSX
fullScreenDecorator child = do
    themeProvider <- mkThemeProvider
    pure
      $ R.div
          { children:
            [ element themeProvider
                { theme: darkTheme
                , children:
                  [ R.div
                      { style:
                        css
                          { width: "100%"
                          , height: "100vh"
                          , backgroundColor: darkTheme.backgroundColour
                          }
                      , children:
                        [ R.div
                            { children:
                              [ child ]
                            , style:
                              css
                                { width: "500px"
                                , padding: "80px"
                                }
                            }
                        ]
                      }
                  ]
                }
            ]
          }
