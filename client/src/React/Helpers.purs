module React.Helpers where

import Prelude

import Data.Nullable (Nullable)
import Data.Symbol (SProxy(..))
import Foreign.Object (Object)
import Prim.Row (class Lacks, class Union)
import React.Basic (JSX)
import React.Basic.DOM (CSS)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Record (insert)
import Web.DOM (Node)

wrapperDiv ∷ ∀ attrs attrs_.
  Lacks "children" attrs =>
  Union attrs attrs_ Props_div_no_children =>
  {|attrs}
  -> JSX
  -> JSX
wrapperDiv props child =
  R.div $ insert (SProxy ∷ _ "children") [child] props

type Props_div_no_children =
  ( _data ∷ Object String
  , about ∷ String
  , acceptCharset ∷ String
  , accessKey ∷ String
  , allowFullScreen ∷ Boolean
  , allowTransparency ∷ Boolean
  , autoComplete ∷ Boolean
  , autoFocus ∷ Boolean
  , autoPlay ∷ Boolean
  , capture ∷ Boolean
  , cellPadding ∷ String
  , cellSpacing ∷ String
  , charSet ∷ String
  , classID ∷ String
  , className ∷ String
  , colSpan ∷ Int
  , contentEditable ∷ Boolean
  , contextMenu ∷ String
  , crossOrigin ∷ String
  , dangerouslySetInnerHTML ∷ { __html ∷ String }
  , datatype ∷ String
  , dateTime ∷ String
  , dir ∷ String
  , draggable ∷ Boolean
  , encType ∷ String
  , formAction ∷ String
  , formEncType ∷ String
  , formMethod ∷ String
  , formNoValidate ∷ Boolean
  , formTarget ∷ String
  , frameBorder ∷ String
  , hidden ∷ Boolean
  , hrefLang ∷ String
  , htmlFor ∷ String
  , httpEquiv ∷ String
  , icon ∷ String
  , id ∷ String
  , inlist ∷ String
  , inputMode ∷ String
  , is ∷ String
  , itemID ∷ String
  , itemProp ∷ String
  , itemRef ∷ String
  , itemScope ∷ Boolean
  , itemType ∷ String
  , key ∷ String
  , keyParams ∷ String
  , keyType ∷ String
  , lang ∷ String
  , marginHeight ∷ String
  , marginWidth ∷ String
  , maxLength ∷ Int
  , mediaGroup ∷ String
  , minLength ∷ Int
  , noValidate ∷ Boolean
  , onAnimationEnd ∷ EventHandler
  , onAnimationIteration ∷ EventHandler
  , onAnimationStart ∷ EventHandler
  , onBlur ∷ EventHandler
  , onClick ∷ EventHandler
  , onCompositionEnd ∷ EventHandler
  , onCompositionStart ∷ EventHandler
  , onCompositionUpdate ∷ EventHandler
  , onContextMenu ∷ EventHandler
  , onCopy ∷ EventHandler
  , onCut ∷ EventHandler
  , onDoubleClick ∷ EventHandler
  , onDrag ∷ EventHandler
  , onDragEnd ∷ EventHandler
  , onDragEnter ∷ EventHandler
  , onDragExit ∷ EventHandler
  , onDragLeave ∷ EventHandler
  , onDragOver ∷ EventHandler
  , onDragStart ∷ EventHandler
  , onDrop ∷ EventHandler
  , onFocus ∷ EventHandler
  , onGotPointerCapture ∷ EventHandler
  , onInvalid ∷ EventHandler
  , onKeyDown ∷ EventHandler
  , onKeyPress ∷ EventHandler
  , onKeyUp ∷ EventHandler
  , onLostPointerCapture ∷ EventHandler
  , onMouseDown ∷ EventHandler
  , onMouseEnter ∷ EventHandler
  , onMouseLeave ∷ EventHandler
  , onMouseMove ∷ EventHandler
  , onMouseOut ∷ EventHandler
  , onMouseOver ∷ EventHandler
  , onMouseUp ∷ EventHandler
  , onPaste ∷ EventHandler
  , onPointerCancel ∷ EventHandler
  , onPointerDown ∷ EventHandler
  , onPointerEnter ∷ EventHandler
  , onPointerLeave ∷ EventHandler
  , onPointerMove ∷ EventHandler
  , onPointerOut ∷ EventHandler
  , onPointerOver ∷ EventHandler
  , onPointerUp ∷ EventHandler
  , onSelect ∷ EventHandler
  , onSubmit ∷ EventHandler
  , onTouchCancel ∷ EventHandler
  , onTouchEnd ∷ EventHandler
  , onTouchMove ∷ EventHandler
  , onTouchStart ∷ EventHandler
  , onTransitionEnd ∷ EventHandler
  , onWheel ∷ EventHandler
  , prefix ∷ String
  , property ∷ String
  , radioGroup ∷ String
  , readOnly ∷ Boolean
  , ref ∷ Ref (Nullable Node)
  , resource ∷ String
  , role ∷ String
  , rowSpan ∷ Int
  , scoped ∷ Boolean
  , seamless ∷ Boolean
  , security ∷ String
  , spellCheck ∷ Boolean
  , srcDoc ∷ JSX
  , srcLang ∷ String
  , srcSet ∷ String
  , style ∷ CSS
  , suppressContentEditableWarning ∷ Boolean
  , tabIndex ∷ Int
  , title ∷ String
  , typeof ∷ String
  , unselectable ∷ Boolean
  , useMap ∷ String
  , vocab ∷ String
  , wmode ∷ String
  )
