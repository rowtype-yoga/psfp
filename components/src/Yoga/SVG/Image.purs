module Yoga.SVG.Image where

import Prelude
import Color (toHexString)
import Data.Array (elem, foldl, head, intercalate, snoc, uncons, zip, (:))
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import JSS (jssClasses)
import Random.PseudoRandom (mkSeed, randomRs)
import React.Basic (JSX, element)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as HTML
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (ReactComponent)
import React.Basic.Hooks as React
import Yoga.DOM.Hook (useBoundingBox)
import Yoga.SVG.Icon (Raw)
import Yoga.Scroll.Hook (useScrollYPosition)
import Yoga.Theme.Styles (makeStylesJSS, useTheme)
import Yoga.Theme.Types (CSSTheme)

foreign import landingPageDarkRaw ∷ ∀ r. Raw r

foreign import landingPageLightRaw ∷ ∀ r. Raw r

mkLandingPageBackground ∷ Effect (ReactComponent { className ∷ String })
mkLandingPageBackground = do
  useStyles <-
    makeStylesJSS
      $ jssClasses \(theme ∷ CSSTheme) ->
          { laptopBackground:
            { fill: theme.backgroundColour # toHexString
            }
          , lightEllipsis:
            { cx: if theme.isLight then "721" else "175"
            , cy: if theme.isLight then "355" else "310"
            , rx: if theme.isLight then "647" else "330"
            , ry: if theme.isLight then "310" else "252"
            }
          , laptopBody:
            { fill: if theme.isLight then "url(#_Linear9)" else "url(#_Linear12)"
            }
          , screenText:
            { fontFamily: theme.codeFontFamily
            , fontSize: "0.85em"
            , fill: theme.textColour # toHexString
            }
          , keyword: { fill: theme.green # toHexString }
          , typeName: { fill: theme.pink # toHexString }
          , "@keyframes animatedCursor":
            { "0%": { opacity: "0.0" }
            , "50%": { opacity: "1.0" }
            }
          , cursor:
            { animation: "$animatedCursor 0.5s linear infinite"
            , animationDirection: "alternate"
            }
          , "@keyframes movingCloud1":
            { "0%": { transform: "translate3d(-140%, 4%, 0)" }
            , "100%": { transform: "translate3d(70%, -4%, 0)" }
            }
          , movingCloud1:
            { animation: "$movingCloud1 200s linear infinite"
            , animationDelay: "-150s"
            }
          , "@keyframes movingCloud2":
            { "0%": { transform: "translate3d(-10%, 0%, 0)" }
            , "100%": { transform: "translate3d(10%, 0%, 0)" }
            }
          , movingCloud2:
            { animation: "$movingCloud2 120s linear infinite"
            , animationDirection: "alternate"
            }
          , movingCloud3:
            { animation: "$movingCloud2 120s linear infinite"
            , animationDelay: "-120s"
            , animationDirection: "alternate"
            }
          , "@keyframes twinkle":
            { "0%": { fill: "rgba(250, 250, 250, 0.2)" }
            , "50%": { fill: "rgba(250, 250, 250, 0.7)" }
            , "70%": { fill: "rgba(250, 250, 250, 0.4)" }
            , "100%": { fill: "rgba(250, 250, 250, 1.0)" }
            }
          , "@keyframes twinkleFast":
            { "0%": { fill: "rgba(250, 250, 250, 0.7)" }
            , "100%": { fill: "rgba(250, 250, 250, 0.78)" }
            }
          , twinklingStar1:
            { animation: "$twinkleFast 0.2s ease-in-out infinite"
            , animationDirection: "alternate"
            }
          , twinklingStar2:
            { animation: "$twinkle 8s ease-in-out infinite"
            , animationDelay: "4s"
            , animationDirection: "alternate"
            }
          , twinklingStar3:
            { animation: "$twinkle 4s ease-in-out infinite"
            , animationDelay: "3s"
            , animationDirection: "alternate"
            }
          , "@keyframes rotateStars":
            { "0%": { transform: "rotate(0deg) scale(1,1)" }
            , "100%": { transform: "rotate(1deg) scale(1.01, 1.01)" }
            }
          , stars:
            { animation: "$rotateStars 30s linear infinite"
            , transformOrigin: "top center"
            , animationDirection: "alternate"
            }
          }
  React.component "LandingPageBackground" \{ className } -> React.do
    theme <- useTheme
    classes <- useStyles {}
    scrollY <- useScrollYPosition
    bb /\ ref <- useBoundingBox
    let
      scrolled = case bb of
        Nothing -> 0.0
        Just vh -> scrollY / vh.height
    pure
      $ element (unsafeCreateDOMComponent "svg") -- [TODO] make PR for react-basic to allow for `ref` on svg
          { ref
          , viewBox: "0 0 800 447"
          , preserveAspectRatio: "xMidYMax slice"
          , xmlns: "http://www.w3.org/2000/svg"
          , fillRule: "evenodd"
          , clipRule: "evenodd"
          , strokeLinejoin: "round"
          , strokeMiterlimit: "2"
          , className
          , children:
            [ SVG.path
                { fill: "none"
                , d: "M0 0h800v447H0z"
                }
            , SVG.clipPath
                { id: "a"
                , children:
                  [ SVG.path
                      { d: "M0 0h800v447H0z"
                      }
                  ]
                }
            , SVG.g
                { clipPath: "url(#a)"
                , children:
                  [ SVG.path
                      { d: "M800.828 448.051V-3.513H-3.028v451.564h803.856z"
                      , fill: "url(#_Linear2)"
                      }
                  , SVG.ellipse
                      { className: classes.lightEllipsis
                      , fill: "url(#_Radial3)"
                      }
                  , stars classes.stars classes
                  -- bgcloud
                  , SVG.path
                      { d: "M610.291 177.316c-35.879-.428-84.67-.691-138.392-.689-38.684.002-74.811.141-105.402.38-49.14-2.745-80.877-9.954-80.877-14.718 0-1.333 2.655.335 7.294-.896 96.772-.175 171.302-1.213 171.302-2.464 0-1.107-59.462-3.372-140.106-3.687 38.487-2.059 102.543-4.972 173.823-4.972 11.996 0 23.761.07 35.216.206 20.76-4.638 60.945-2.784 107.07-2.784 36.08 0 68.525-.075 90.882 2.983 32.491.899 72.142-1.186 114.922-.379 38.68.729 74.805 1.273 105.395 1.613 49.101 3.673 80.599 8.516 80.54 13.279-.016 1.334-2.504 2.58-7.158 3.723-96.763-1.654-171.297-2.024-171.312-.774-.013 1.108 58.473 3.15 139.105 4.989-38.514 3.085-101.653 4.38-172.925 3.033-11.995-.227-23.757-.52-35.209-.872-20.815 4.245-61.034 6.631-107.154 5.759-24.754-.468-47.786-1.809-67.014-3.73z"
                      , fill: if theme.isLight then "#ddf0fb" else "#240c3c"
                      , className: classes.movingCloud1
                      , transform: "translate(0 " <> show (scrolled * 20.0) <> ")"
                      }
                  -- bgcloud
                  , SVG.path
                      { d: "M609.299 171.607c-35.88-.338-84.67-.545-138.392-.543-38.684.001-74.812.111-105.402.299-49.141-2.166-80.694-5.519-80.694-9.279 0-1.052 2.472-2.073 7.111-3.045 96.772-.137 171.302-.957 171.302-1.944-.001-.874-58.506-1.614-139.15-1.862 38.471-3.009 101.587-4.973 172.867-4.973 11.996 0 23.761.055 35.216.162 20.76-3.66 60.945-6.143 107.07-6.143 36.08 0 68.525 1.519 90.882 3.933 32.491.709 72.142 1.432 114.922 2.069 38.68.575 74.805 1.005 105.395 1.272 49.101 2.9 80.598 6.722 80.54 10.482-.017 1.052-2.505 2.036-7.158 2.938-96.763-1.305-171.297-1.597-171.312-.611-.014.875 58.473 2.486 139.104 3.938-38.514 2.435-101.653 3.457-172.924 2.394a3771.84 3771.84 0 01-35.21-.688c-20.814 3.35-61.033 5.234-107.153 4.545-24.754-.369-47.786-1.428-67.014-2.944z"
                      , fill: if theme.isLight then "#d8effd" else "#120b38"
                      , className: classes.movingCloud1
                      , transform: "translate(0 " <> show (scrolled * 20.0) <> ")"
                      }
                  -- rightcloud
                  , SVG.path
                      { d: "M684.234 222.269c-12.108-1.642-29.721-2.832-29.76-5.218-.063-3.861 34.226-4.225 76.6-3.504 42.375.721 75.9-2.707 76.956 1.008 2.476 8.704-12.549 8.552-32.56 10.141-12.403-.58-28.24.216-44.434.479-16.193.263-35.197-1.331-46.802-2.906z"
                      , fill: if theme.isLight then "#d8effd" else "url(#_Linear4)"
                      , className: classes.movingCloud2
                      , transform: "translate(0 " <> show (scrolled * 20.0) <> ")"
                      }
                  -- leftcloud
                  , SVG.path
                      { d: "M130.064 213.821c42.381 0 76.789.237 76.789 4.224 0 3.986-14.408 4.223-56.789 4.223-42.38 0-96.788.208-96.788-4.223s34.408-4.224 76.788-4.224z"
                      , fill: if theme.isLight then "#d8effd" else "#a7847d"
                      , className: classes.movingCloud3
                      , transform: "translate(0 " <> show (scrolled * 20.0) <> ")"
                      }
                  , mountains theme.isLight scrolled
                  --buildings
                  , buildings theme.isLight scrolled
                  -- bar
                  , SVG.path
                      { fill: theme.backgroundColour # toHexString
                      , d: "M-3.028 431.298h826.844v20.006H-3.028z"
                      }
                  -- laptop
                  , laptop classes scrolled
                  ]
                }
            , defs theme.isLight
            ]
          }

laptop ∷
  ∀ r.
  { cursor ∷ String
  , keyword ∷ String
  , laptopBackground ∷ String
  , laptopBody ∷ String
  , screenText ∷ String
  , typeName ∷ String
  | r
  } ->
  Number -> JSX
laptop classes scrolled =
  SVG.g
    { children:
      [ SVG.path
          { d: "M606.064 273.603c0-6.261-5.076-11.337-11.337-11.337H399.673c-6.261 0-11.337 5.076-11.337 11.337v151.521a5.001 5.001 0 005 5h207.728a5.004 5.004 0 003.536-1.464 5.004 5.004 0 001.464-3.536V273.603z"
          , fill: "#657390"
          }
      , SVG.path
          { d: "M606.064 273.603c0-6.261-5.076-11.337-11.337-11.337H399.673c-6.261 0-11.337 5.076-11.337 11.337v151.521a5.001 5.001 0 005 5h207.728a5.004 5.004 0 003.536-1.464 5.004 5.004 0 001.464-3.536V273.603z"
          , fill: "#657390"
          }
      , SVG.path
          { d: "M599.193 275.905a5.528 5.528 0 00-5.526-5.526H400.604a5.528 5.528 0 00-5.526 5.526v141.039a5.528 5.528 0 005.526 5.526h193.063a5.528 5.528 0 005.526-5.526V275.905z"
          , fill: "#130c2b"
          , className: classes.laptopBackground
          }
      , SVG.path
          { d: "M655.04 427.228a.458.458 0 00-.457-.457H340.83v4.975a5 5 0 005 5h304.21a5 5 0 005-5v-4.518z"
          , className: classes.laptopBody
          }
      , SVG.circle
          { cx: "497.178"
          , cy: "267.001"
          , r: "1.238"
          , fillOpacity: ".45"
          }
      , SVG.circle
          { cx: "500.009"
          , cy: "267.001"
          , r: ".527"
          , fillOpacity: ".45"
          }
      , SVG.circle
          { cx: "494.263"
          , cy: "267.001"
          , r: ".527"
          , fill: "#102708"
          , fillOpacity: ".45"
          }
      , SVG.text
          { x: "407"
          , y: "290"
          , className: classes.screenText
          , children:
            textToTspans classes scrolled 407.0
              $ intercalate "\n"
                  [ "hoistJoker "
                  , "\t∷  ∀ f g a b"
                  , "\t.  (f ~> g)"
                  , "\t-> Joker f a b"
                  , "\t-> Joker g a b"
                  , "hoistJoker f (Joker a) ="
                  , "\t Joker (f a)"
                  ]
          }
      ]
    }

buildings ∷ Boolean -> Number -> JSX
buildings isLight scrolled =
  SVG.g
    { transform: "translate(0 " <> show (scrolled * 30.0) <> ")"
    , children:
      [ SVG.path -- 2 main
          { d: "M195.171 451.298V159.153H92.709v292.145h102.462z"
          , fill: if isLight then "url(#_Linear4)" else "url(#_Linear5)"
          }
      , SVG.path -- 4 main
          { d: "M289.755 451.298V232.811h-57.232v218.487h57.232z"
          , fill: if isLight then "url(#_Linear5)" else "url(#_Linear6)"
          }
      , SVG.path -- 4 shade
          { d: "M207.557 451.298V232.811h25.314v218.487h-25.314zM92.709 451.298V159.153h25.314v292.145H92.709z"
          , fill: if isLight then "#b4dbfb" else "#130425"
          }
      , SVG.path -- 5 shade
          { d: "M323.451 451.298V327.713h-71.555v123.585h71.555z"
          , fill: if isLight then "url(#_Linear6)" else "url(#_Linear7)"
          , transform: "translate(0 " <> show (scrolled * 60.0) <> ")"
          }
      , SVG.path -- 5 main
          { d: "M323.451 451.298V327.713h-16.692v123.585h16.692z"
          , fill: if isLight then "url(#_Linear7)" else "url(#_Linear8)"
          , transform: "translate(0 " <> show (scrolled * 60.0) <> ")"
          }
      , SVG.path -- 3 main
          { d: "M272.498 451.298v-95.771H135.943v95.771h136.555z"
          , fill: if isLight then "url(#_Linear8)" else "url(#_Linear9)"
          , transform: "translate(0 " <> show (scrolled * 100.0) <> ")"
          }
      , SVG.path -- 3 shade
          { d: "M173.526 451.298v-95.771h-37.583v95.771h37.583z"
          , fill: if isLight then "#b4dbfb" else "#2e1232"
          , transform: "translate(0 " <> show (scrolled * 100.0) <> ")"
          }
      , SVG.path -- 1 main
          { d: "M85.122 451.298V272.442h-97.675v178.856h97.675z"
          , fill: if isLight then "#b4dbfb" else "url(#_Linear10)"
          , transform: "translate(0 " <> show (scrolled * 100.0) <> ")"
          }
      , SVG.path -- 1 shade
          { d: "M66.649 456.944V272.442H-3.028v184.502h69.677z"
          , fill: if isLight then "#b4dbfb" else "url(#_Linear11)"
          , transform: "translate(0 " <> show (scrolled * 100.0) <> ")"
          }
      ]
    }

mountains ∷ Boolean -> Number -> JSX
mountains isLight scrolled =
  SVG.g
    { transform: "translate(0 " <> show (scrolled * 70.0) <> ")"
    , children:
      [ SVG.path
          { d: "M476.098 218.299l81.644 216.765H352.334l123.764-216.765z"
          , fill: if isLight then "#4560a0" else "#04102d"
          }
      , SVG.path
          { d: "M476.098 218.299l21.823 203.477 81.177-8.706-103-194.771z"
          , fill: if isLight then "#5e84ba" else "#250849"
          }
      -- 2 dark (left)
      , SVG.path
          { d: "M369.627 308.648l88.326 125.788H252.546l117.081-125.788z"
          , fill: if isLight then "#4560a0" else "#04102d"
          }
      -- 1 dark (left)
      , SVG.path
          { d: "M326.11 338.332l68.193 96.732H188.896l137.214-96.732zM508.168 243.792l163.526 204.747H415.051l93.117-204.747z"
          , fill: if isLight then "#4560a0" else "#04102d"
          }
      , SVG.path -- 1 and 2 light (right)
          { d: "M508.168 245.355l63.395 75.115-54.953 46.379-8.442-121.494zM369.612 308.648l54.182 77.687-58.981 41.135 4.799-118.822zM326.184 338.326l16.813 23.99-16.887 11.835.074-35.825z"
          , fill: if isLight then "#5e84ba" else "#250849"
          }
      -- right mountain
      , SVG.path -- rightmost dark (left)
          { d: "M633.647 230.42L809.34 435.064H457.953L633.647 230.42z"
          , fill: if isLight then "#4560a0" else "#04102d"
          }
      , SVG.path -- rightmost light (right)
          { d: "M633.647 230.42L735.22 475.549l86.041-35.653L633.647 230.42z"
          , fill: if isLight then "#5e84ba" else "#250849"
          }
      , SVG.path -- peak left
          { d: "M643.625 254.384c-.571.219-.745.283-1.863.82-2.864 1.395-6.079 3.209-9.184 2.64-6.187-1.134-10.854-6.283-16.725-6.738l17.786-20.729 9.986 24.007z"
          , fill: if isLight then "#d9d9d9" else "#655578"
          }
      , SVG.path -- peak right
          { d: "M654.229 253.4c-.494-.028-.875-.013-.875-.013-3.255-.486-6.617-.192-9.677.982l-.052.02-9.978-23.97 20.582 22.981z"
          , fill: if isLight then "#fff" else "#9d779e"
          }
      ]
    }

textToTspans ∷
  ∀ r.
  { cursor ∷ String
  , keyword ∷ String
  , typeName ∷ String
  | r
  } ->
  Number -> Number -> String -> Array JSX
textToTspans classes scrolled x str = (foldl toTspan { distance: 0.0, acc: [] } lines).acc <> [ cursor ]
  where
  toTspan { distance, acc } line = case uncons (words line) of
    Nothing -> { distance: distance + 1.2, acc }
    Just { head: "", tail: [] } -> { distance: distance + 1.2, acc }
    Just { head, tail } -> do
      let
        newElems ∷ Array JSX
        newElems = _.spans $ foldl mkNewElems { prefix: " ", spans: [] } tail

        mkNewElems { prefix, spans } = case _ of
          "" -> { prefix: prefix <> " ", spans }
          word ->
            { prefix: " "
            , spans:
              spans
                `snoc`
                  SVG.tspan
                    { children: [ HTML.text $ prefix <> word ]
                    , className: classNameForWord word
                    }
            }

        first =
          SVG.tspan
            { x: show x
            , dy: show distance <> "em"
            , children: [ HTML.text head ]
            }

        pimped ∷ Array JSX
        pimped = first : newElems
      { distance: 1.2, acc: acc <> pimped }
  classNameForWord = case _ of
    keyword
      | elem keyword keywords -> classes.keyword
    typeName
      | isTypeName typeName -> classes.typeName
    other -> ""
  isTypeName s =
    let
      firstLetter = String.take 1 s

      inRange c1 c2 = between (codePointFromChar c1) (codePointFromChar c2)

      isLetter =
        String.toCodePointArray
          >>> head
          >>> maybe false (inRange 'a' 'z' || inRange 'A' 'Z')
    in
      String.toUpper firstLetter == firstLetter && isLetter firstLetter
  keywords =
    [ "module"
    , "∷"
    , "::"
    , "forall"
    , "∀"
    , "do"
    , "where"
    , "let"
    , "in"
    , "instance"
    , "class"
    , "data"
    , "="
    , "|"
    , "type"
    , "newtype"
    , "derive"
    , "if"
    , "then"
    , "else"
    , "import"
    ]
  words line = split
    where
    split = String.split (String.Pattern " ") line
  lines = String.split (String.Pattern "\n") scrolledString
  scrolledString = String.take charactersToTake str
    where
    doneAfterXPercent = 0.5 -- [WARN] Don't set to 0
    charactersToTake = round $ scrolled * (1.0 / doneAfterXPercent) * len
    len = toNumber $ String.length str
  cursor =
    SVG.tspan
      { dx: "0"
      , dy: "0"
      , children: [ HTML.text "█" ]
      , className: classes.cursor
      , id: "landing-page-cursor"
      }

defs ∷ Boolean -> JSX
defs = if _ then lightDefs else darkDefs

lightDefs ∷ JSX
lightDefs =
  SVG.defs
    { children:
      [ SVG.linearGradient
          { id: "_Linear2"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(0 451.564 -803.856 0 398.9 -3.513)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#68adfc"
                }
            , SVG.stop
                { offset: ".34"
                , stopColor: "#cbe0f8"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#d2cbf8"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear4"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(293.8304) rotate(83.86 -.11 .484)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#bde8f7"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#86b6fe"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear5"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(219.3256) rotate(84.988 -.028 1.133)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#bde8f7"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#86b6fe"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear6"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "rotate(90 -6.304 321.409) scale(123.585)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#bde8f7"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#86b6fe"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear7"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(132.4342) rotate(68.9 -1.112 2.415)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#bde8f7"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#86b6fe"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear8"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "rotate(90 -88.788 166.635) scale(190.951)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#bde8f7"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#86b6fe"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear9"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(314.21 0 0 9.97478 340.83 431.758)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#a9abc1"
                }
            , SVG.stop
                { offset: ".11"
                , stopColor: "#65687d"
                }
            , SVG.stop
                { offset: ".5"
                , stopColor: "#c3c5d2"
                }
            , SVG.stop
                { offset: ".89"
                , stopColor: "#64677c"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#a4a7c2"
                }
            ]
          }
      , SVG.radialGradient
          { id: "_Radial3"
          , cx: "0"
          , cy: "0"
          , r: "1"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(647.316 0 0 309.935 720.558 355.142)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#cfcfcf"
                , stopOpacity: ".65"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#fff552"
                , stopOpacity: "0"
                }
            ]
          }
      ]
    }

darkDefs ∷ JSX
darkDefs =
  SVG.defs
    { children:
      [ SVG.linearGradient
          { id: "_Linear2"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(0 451.564 -803.856 0 398.9 -3.513)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#000434"
                }
            , SVG.stop
                { offset: ".28"
                , stopColor: "#171043"
                }
            , SVG.stop
                { offset: ".68"
                , stopColor: "#573774"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#b8734e"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear4"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(-.53832 -17.3125 1.44013 -.04478 749.812 228.864)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#e59f77"
                }
            , SVG.stop
                { offset: ".47"
                , stopColor: "#a46075"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#914d74"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear5"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(293.8304) rotate(83.86 -.11 .484)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#08001b"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#571547"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear6"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(219.3256) rotate(84.988 -.028 1.133)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#000418"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#571548"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear7"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(46.7869 123.585 -71.5547 27.0893 259.972 327.713)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#000418"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#571548"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear8"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "rotate(90 -6.304 321.409) scale(123.585)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#3a0843"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#9f5c6a"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear9"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(101.643) rotate(28.107 -6.187 4.945)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#2c1d2f"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#480445"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear10"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "scale(-106.68957) rotate(-86.136 -1.645 -.807)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#2c1d2f"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#480445"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear11"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(20.5602 0 -184.503 0 11.25 272.442)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#000624"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#4f1557"
                }
            ]
          }
      , SVG.linearGradient
          { id: "_Linear12"
          , x1: "0"
          , y1: "0"
          , x2: "1"
          , y2: "0"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(314.21 0 0 9.97478 340.83 431.758)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#a9abc1"
                }
            , SVG.stop
                { offset: ".11"
                , stopColor: "#65687d"
                }
            , SVG.stop
                { offset: ".5"
                , stopColor: "#c3c5d2"
                }
            , SVG.stop
                { offset: ".89"
                , stopColor: "#64677c"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#a4a7c2"
                }
            ]
          }
      , SVG.radialGradient
          { id: "_Radial3"
          , cx: "0"
          , cy: "0"
          , r: "1"
          , gradientUnits: "userSpaceOnUse"
          , gradientTransform: "matrix(330.479 0 0 252.189 174.599 310.24)"
          , children:
            [ SVG.stop
                { offset: "0"
                , stopColor: "#f60"
                , stopOpacity: ".3"
                }
            , SVG.stop
                { offset: "1"
                , stopColor: "#fff552"
                , stopOpacity: "0"
                }
            ]
          }
      ]
    }

infixr 8 zip as <%>

stars ∷ ∀ css. String -> { twinklingStar1 ∷ String, twinklingStar2 ∷ String, twinklingStar3 ∷ String | css } -> JSX
stars className classes =
  let
    n = 30

    xs = randomRs 0.0 800.0 n $ mkSeed 84

    ys = randomRs (-20.0) 200.0 n $ mkSeed 4398

    radiuses = randomRs 0.1 1.3 n (mkSeed 15)

    opacities = randomRs 0.5 0.9 n (mkSeed 92)

    classNames =
      randomRs 1 50 n (mkSeed 3)
        <#> case _ of
            1 -> classes.twinklingStar1
            2 -> classes.twinklingStar2
            3 -> classes.twinklingStar2
            _ -> ""

    zipped = xs <%> ys <%> radiuses <%> classNames <%> opacities

    children = do
      cx /\ cy /\ r /\ className' /\ opacity <- zipped
      pure
        $ SVG.circle
            { cx: show cx
            , cy: show cy
            , r: show r
            , fill: "#eeefff"
            , className: className'
            , opacity: show (opacity - cy / 200.0)
            }
  in
    SVG.g
      { fillOpacity: ".67"
      , children
      , className
      }
