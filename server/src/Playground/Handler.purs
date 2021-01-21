module Playground.Handler where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getBody')
import Node.Express.Response as Response
import Playground.Playground (asErrorWithCode, compileCode, runCode)
import Shared.Json (readAff)
import Shared.Models.Body (RunResult)
import Shared.Models.Body as Body
import Simple.JSON (write)

toBody ∷ ∀ r m. MonadEffect m => { stdout ∷ Buffer, stderr ∷ Buffer | r } -> m RunResult
toBody result = liftEffect $ ado
  stdout <- Buffer.toString UTF8 result.stdout
  stderr <- Buffer.toString UTF8 result.stderr
  let (code ∷ Maybe Int) = asErrorWithCode result >>= _.code
  in { code, stdout, stderr } ∷ RunResult

compileHandler ∷ HandlerM Unit
compileHandler = do
  body <- getBody'
  json <- readAff body # liftAff
  result <- compileCode (json ∷ Body.CompileRequest).code # liftAff
  Response.send $ write ({ result } ∷ Body.CompileResult)

runHandler ∷ HandlerM Unit
runHandler = do
  result <- liftAff do
    result <- runCode
    toBody result
  Response.send $ write (result ∷ Body.RunResult)
