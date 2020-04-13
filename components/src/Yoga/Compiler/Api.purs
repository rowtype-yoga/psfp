module Yoga.Compiler.Api where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt, error, message, throwError)
import Milkis as M
import Milkis.Impl (FetchImpl)
import Shared.Json (readAff)
import Shared.Models.Body as Body
import Simple.JSON (writeJSON)
import Yoga.Compiler.Types (Compiler)

apiCompiler ∷ FetchImpl -> { | Compiler () }
apiCompiler fetchImpl =
  { compileAndRun
  }
  where
  fetch = M.fetch fetchImpl
  compileAndRun ∷ Body.CompileRequest -> Aff (Either Body.CompileResult Body.RunResult)
  compileAndRun body = do
    response <-
      attempt
        $ fetch (M.URL "/api/compileAndRun")
            { method: M.postMethod
            , body: writeJSON body
            , headers: M.makeHeaders { "Content-Type": "application/json" }
            }
    case response of
      Left l ->
        pure
          ( Left
              { resultType: ""
              , result:
                [ { allSpans: []
                  , errorCode: ""
                  , errorLink: ""
                  , filename: ""
                  , message: message l
                  , moduleName: Nothing
                  , position:
                    { endColumn: 0
                    , endLine: 0
                    , startColumn: 0
                    , startLine: 0
                    }
                  , suggestion: Nothing
                  }
                ]
              }
          )
      Right r -> case M.statusCode r of
        200 -> M.json r >>= readAff <#> Right
        422 -> M.json r >>= readAff <#> Left
        code -> throwError (error $ "Unexpected response code " <> show code)
