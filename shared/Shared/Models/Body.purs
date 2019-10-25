module Shared.Models.Body where

import Data.Maybe (Maybe)

type CompileRequest = { code ∷ String }

type CompileResult = { result ∷ String }

type RunResult = { code ∷ Maybe Int, stdout ∷ String, stderr ∷ String }
