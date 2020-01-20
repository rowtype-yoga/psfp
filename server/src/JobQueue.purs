module JobQueue where

import Prelude

import Data.Array (delete, filter, find, length, snoc, zip)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, un)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, error, killFiber, launchAff, launchAff_, parallel, sequential)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype JobId
  = JobId UUID

derive instance ntJobId :: Newtype JobId _
derive instance genericJobId :: Generic JobId _
instance showJobId :: Show JobId where
  show = genericShow

derive newtype instance jobIdEq :: Eq JobId
newtype NewJob a
  = NewJob (a -> Aff Unit)

data PendingJob a
  = PendingJob
    { id ∷ JobId
    , job ∷ a -> Aff Unit
    , addedAt ∷ JSDate
    }

data Job a
  = Pending (PendingJob a)
  | Running (RunningJob a)

getAddedAt ∷ ∀ a. Job a -> JSDate
getAddedAt = case _ of
  Pending (PendingJob { addedAt }) -> addedAt
  Running (RunningJob { addedAt }) -> addedAt

instance eqJob :: Eq (Job a) where
  eq j1 j2 = getId j1 == getId j2

instance ordJob :: Ord (Job a) where
  compare j1 j2 = compare (getAddedAt j1) (getAddedAt j2)

getId ∷ ∀ a. Job a -> JobId
getId = case _ of
  Pending (PendingJob { id }) -> id
  Running (RunningJob { id }) -> id

newtype RunningJob a
  = RunningJob
  { id ∷ JobId
  , addedAt ∷ JSDate
  , startedAt ∷ JSDate
  , fiber ∷ Fiber Unit
  , resource ∷ a
  }

type QueueParams
  = { maxSize ∷ Int
    , timeout ∷ Milliseconds
    }

newtype ResourcePool a
  = ResourcePool (Array a)

derive instance ntResourcePool :: Newtype (ResourcePool a) _
data Queue a
  = Queue QueueParams (Ref (ResourcePool a)) (Ref (Array (Job a)))

mkQueue ∷ ∀ m a. MonadEffect m => QueueParams -> ResourcePool a -> m (Queue a)
mkQueue params pool =
  liftEffect do
    poolRef <- Ref.new pool
    jobsRef <- Ref.new []
    pure $ Queue params poolRef jobsRef

data EnqueueResult
  = Enqueued JobId
  | QueueFull

derive instance eqEnqueueResult :: Eq EnqueueResult
derive instance genericEnqueueResult :: Generic EnqueueResult _
instance showEnqueueResult :: Show EnqueueResult where
  show = genericShow

startJob ∷ ∀ a. Eq a => Queue a -> a -> PendingJob a -> Effect Unit
startJob q@(Queue { timeout } _ jobsRef) resource (PendingJob { id, job, addedAt }) = do
  startedAt <- JSDate.now
  iso <- JSDate.toISOString startedAt
  --   log $ "Starting Job: " <> toString (unwrap id) <> " at: " <> iso
  let
    remove = removeFromQueue id q
  fiber <-
    launchAff <<< sequential
      $ parallel (job resource *> remove)
      *> parallel (delay timeout *> remove)
  let
    running = RunningJob { id, fiber, addedAt, startedAt, resource }
  jobsRef
    # Ref.modify_ (map \j -> if getId j == id then Running running else j)

jobsInQueue ∷ ∀ a. Queue a -> Effect Int
jobsInQueue (Queue _ _ jobsRef) = do
  jobs <- Ref.read jobsRef
  pure $ length jobs

runningJobs ∷ ∀ a. Queue a -> Effect (Array (RunningJob a))
runningJobs (Queue _ resourceRef jobsRef) = do
  jobs <- Ref.read jobsRef
  pure do
    job <- jobs
    case job of
      Running rj -> [ rj ]
      Pending _ -> []

pendingJobs ∷ ∀ a. Queue a -> Effect (Array (PendingJob a))
pendingJobs (Queue _ resourceRef jobsRef) = do
  jobs <- Ref.read jobsRef
  pool <- Ref.read resourceRef
  pure do
    job <- jobs
    case job of
      Running _ -> []
      Pending pj -> [ pj ]

startJobs ∷ ∀ a. Eq a => Queue a -> Effect Unit
startJobs q@(Queue { timeout } poolRef jobsRef) = do
  pool <- Ref.read poolRef
  pending <- pendingJobs q
  for_ (pending `zip` un ResourcePool pool) \(job /\ resource) -> do
    Ref.modify_ (over ResourcePool $ delete resource) poolRef
    startJob q resource job

removeStaleJobs ∷ ∀ a. Eq a => Queue a -> Effect Unit
removeStaleJobs q@(Queue { timeout } resourceRef jobsRef) = do
  running <- runningJobs q
  nowMillis <- JSDate.now <#> JSDate.getTime
  for_ running
    $ \(RunningJob { startedAt, id }) -> do
        let
          runningSince = (nowMillis - JSDate.getTime startedAt) # Milliseconds

          timedOut = runningSince > timeout
        when timedOut $ launchAff_ (removeFromQueue id q)

removeFromQueue ∷ ∀ a. Eq a => JobId -> Queue a -> Aff Unit
removeFromQueue id q@(Queue _ poolRef jobsRef) = do
  maybeJob <- Ref.read jobsRef <#> find (\x -> (getId x) == id) # liftEffect
  -- remove from queue
  jobsRef # Ref.modify_ (filter (\x -> (getId x) /= id)) # liftEffect
  case maybeJob of
    Just (Running (RunningJob { fiber })) -> killFiber (error "Removed from queue") fiber
    _ -> pure unit
  -- free resource
  case maybeJob of
    Just (Running (RunningJob { resource })) -> do
      poolRef # Ref.modify_ (over ResourcePool (_ `snoc` resource)) # liftEffect
    _ -> pure unit
  startJobs q # liftEffect

--| Add to the end of the queue. If the queue is full the result will be `false`
enqueue ∷ ∀ a. Eq a => NewJob a -> Queue a -> Effect EnqueueResult
enqueue (NewJob newJob) q@(Queue { maxSize, timeout } _ jobsRef) = do
  id <- genUUID <#> JobId
  jobs <- Ref.read jobsRef
  if length jobs >= maxSize then
    pure QueueFull
  else do
    addedAt <- JSDate.now
    let
      new = Pending (PendingJob { id, addedAt, job: newJob })
    jobsRef # Ref.modify_ (_ `snoc` new)
    startJobs q
    pure (Enqueued id)
