module Yoga.Compiler.Types where

import Data.Either (Either)
import Effect.Aff (Aff)
import Shared.Models.Body as Body

type Compiler r
  = ( compileAndRun ∷
      Body.CompileRequest -> Aff (Either Body.CompileResult Body.RunResult)
    | r
    )
