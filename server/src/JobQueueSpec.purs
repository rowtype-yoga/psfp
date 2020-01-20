module JobQueueSpec where

import Prelude

import Data.Time.Duration (class Duration, Seconds(..), fromDuration)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import JobQueue (EnqueueResult(..), NewJob(..), QueueParams, ResourcePool(..), mkQueue)
import JobQueue as Q
import Main (execCommand)
import Playground.Playground (Folder(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

params ∷ QueueParams
params =
  { maxSize: 10
  , timeout: (1.0 # Seconds) # fromDuration
  }

mkJob ∷ String -> Number -> NewJob Unit
mkJob msg ms = NewJob \_ -> (delay (ms # Milliseconds) *> log msg)

job1 ∷ NewJob Unit
job1 = mkJob "Job 1" 10.0

writeRefJob ∷ ∀ a d. Show a => Duration d => Ref a -> a -> d -> NewJob Unit
writeRefJob ref a d =
  NewJob \_ -> do
    let
      millis ∷ Milliseconds
      millis = fromDuration d
    delay millis
    Ref.write a ref # liftEffect

pool :: ResourcePool Unit
pool = ResourcePool [unit, unit, unit]

spec ∷ Spec Unit
spec = do
  describe "The Queue" do
    it "can't enqueue if maxSize is 0" do
      q <- mkQueue (params { maxSize = 0 }) pool 
      r <- Q.enqueue job1 q # liftEffect
      r `shouldEqual` QueueFull
    it "can enqueue if maxSize is 1" do
      q <- mkQueue (params { maxSize = 1 }) pool
      r <- Q.enqueue job1 q # liftEffect
      r `shouldNotEqual` QueueFull
    it "runsJobs" do
      q <- mkQueue (params { maxSize = 1 }) pool
      ref <- Ref.new false # liftEffect
      r <- Q.enqueue (writeRefJob ref true (0.0 # Seconds)) q # liftEffect
      r `shouldNotEqual` QueueFull
      delay (5.0 # Milliseconds # fromDuration)
      res <- Ref.read ref # liftEffect
      res `shouldEqual` true
    it "kills slow jobs" do
      q <- mkQueue (params { maxSize = 1, timeout = (3.0 # Milliseconds) }) pool
      ref <- Ref.new false # liftEffect
      r <- Q.enqueue (writeRefJob ref true (4.0 # Milliseconds)) q # liftEffect
      r `shouldNotEqual` QueueFull
      res <- Ref.read ref # liftEffect
      delay (9.0 # Milliseconds)
      res `shouldEqual` false
    it "runs multiple jobs" do
      q <- mkQueue (params { maxSize = 3, timeout = (2.0 # Seconds # fromDuration) }) (ResourcePool [unit])
      ref <- Ref.new 0 # liftEffect
      let
        enq n delay = Q.enqueue (writeRefJob ref n delay) q # liftEffect
      r₁ <- enq 1 (0.1 # Seconds)
      r₁ `shouldNotEqual` QueueFull
      r₂ <- enq 2 (0.2 # Seconds)
      r₂ `shouldNotEqual` QueueFull
      r₃ <- enq 3 (0.3 # Seconds)
      r₃ `shouldNotEqual` QueueFull
      Ref.read ref # liftEffect >>= shouldEqual 0 -- ~0ms
      delay (0.1 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 1 -- ~150ms
      delay (0.2 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 2 -- ~250ms
      delay (0.3 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 3 -- ~350ms
    it "respects pool size" do
      q <- mkQueue (params { maxSize = 3, timeout = (2.0 # Seconds # fromDuration) }) (ResourcePool [unit])
      ref <- Ref.new 0 # liftEffect
      let
        enq n delay = Q.enqueue (writeRefJob ref n delay) q # liftEffect
      r₁ <- enq 1 (0.1 # Seconds)
      r₁ `shouldNotEqual` QueueFull
      r₂ <- enq 2 (0.1 # Seconds)
      r₂ `shouldNotEqual` QueueFull
      r₃ <- enq 3 (0.1 # Seconds)
      r₃ `shouldNotEqual` QueueFull
      Ref.read ref # liftEffect >>= shouldEqual 0 -- ~0ms
      delay (0.1 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 1 -- ~150ms
      delay (0.2 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 2 -- ~250ms
      delay (0.3 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 3 -- ~350ms
    it "respects pool size (2)" do
      q <- mkQueue (params { maxSize = 3, timeout = (2.0 # Seconds # fromDuration) }) (ResourcePool [unit, unit, unit])
      ref <- Ref.new 0 # liftEffect
      let
        enq n delay = Q.enqueue (writeRefJob ref n delay) q # liftEffect
      r₁ <- enq 1 (0.1 # Seconds)
      r₂ <- enq 2 (0.1 # Seconds)
      r₃ <- enq 3 (0.1 # Seconds)
      r₁ `shouldNotEqual` QueueFull
      r₂ `shouldNotEqual` QueueFull
      r₃ `shouldNotEqual` QueueFull
      Ref.read ref # liftEffect >>= shouldEqual 0 -- ~0ms
      delay (0.15 # Seconds # fromDuration)
      Ref.read ref # liftEffect >>= shouldEqual 3 -- ~150ms
    it "can kill processes" do
      q <- mkQueue (params { maxSize = 1, timeout = (1.0 # Seconds # fromDuration) }) (ResourcePool [unit])
      resultRef <- Ref.new false # liftEffect
      let
        job =
          NewJob \_ -> do
            res <- execCommand (Folder ".") "sleep 2"
            Ref.write true resultRef # liftEffect
      r₁ <- Q.enqueue job q # liftEffect
      r₁ `shouldNotEqual` QueueFull
      delay (1.3 # Seconds # fromDuration)
      result <- Ref.read resultRef # liftEffect
      result `shouldEqual` false
