module Shared.Models.Body where

import Data.Maybe (Maybe)

type CompileRequest
  = { code ∷ String }

-- type CompileResult
--   = { result ∷ Array ErrorOrWarning 
--     , resultType :: String
--     }
type CompileResult
  = { result ∷ Array ErrorOrWarning
    , resultType ∷ String
    }

type RunResult
  = { code ∷ Maybe Int, stdout ∷ String, stderr ∷ String }

type Suggestion
  = { replaceRange ∷ Position, replacement ∷ String }

type Span
  = { end ∷ Array Int
    , name ∷ String
    , start ∷ Array Int
    }

type Position
  = { endColumn ∷ Int
    , endLine ∷ Int
    , startColumn ∷ Int
    , startLine ∷ Int
    }

type ErrorOrWarning
  = { allSpans ∷ Array Span
    , errorCode ∷ String
    , errorLink ∷ String
    , filename ∷ String
    , message ∷ String
    , moduleName ∷ Maybe String
    , position ∷ Position
    , suggestion ∷ Maybe Suggestion
    }
