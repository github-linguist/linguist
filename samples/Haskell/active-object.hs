module Integrator (
  newIntegrator, input, output, stop,
  Time, timeInterval
) where
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Exception (evaluate)
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- RC task
main = do let f = 0.5 {- Hz -}
          t0 <- getCurrentTime
          i <- newIntegrator
          input i (\t -> sin(2*pi * f * timeInterval t0 t)) -- task step 1
          threadDelay 2000000 {- µs -}                      -- task step 2
          input i (const 0)                                 -- task step 3
          threadDelay 500000 {- µs -}                       -- task step 4
          result <- output i
          stop i
          print result

---- Implementation ------------------------------------------------------

-- Utilities for working with the time type
type Time = UTCTime
type Func a = Time -> a
timeInterval t0 t1 = realToFrac $ diffUTCTime t1 t0

-- Type signatures of the module's interface
newIntegrator :: Fractional a => IO (Integrator a) -- Create an integrator
input  :: Integrator a -> Func a -> IO ()          -- Set the input function
output :: Integrator a           -> IO a           -- Get the current value
stop   :: Integrator a           -> IO ()          -- Stop integration, don't waste CPU

-- Data structures
data Integrator a = Integrator (MVar (IntState a)) -- MVar is a thread-safe mutable cell
  deriving Eq
data IntState a = IntState { func  :: Func a,      -- The current function
                             run   :: Bool,        -- Whether to keep going
                             value :: a,           -- The current accumulated value
                             time  :: Time }       -- The time of the previous update

newIntegrator = do
  now <- getCurrentTime
  state <- newMVar $ IntState { func  = const 0,
                                run   = True,
                                value = 0,
                                time  = now }
  thread <- forkIO (intThread state)  -- The state variable is shared between the thread
  return (Integrator state)           --   and the client interface object.

input  (Integrator stv) f = modifyMVar_ stv (\st -> return st { func = f })
output (Integrator stv)   = fmap value $ readMVar stv
stop   (Integrator stv)   = modifyMVar_ stv (\st -> return st { run = False })
  -- modifyMVar_ takes an MVar and replaces its contents according to the provided function.
  -- a { b = c } is record-update syntax: "the record a, except with field b changed to c"

-- Integration thread
intThread :: Fractional a => MVar (IntState a) -> IO ()
intThread stv = whileM $ modifyMVar stv updateAndCheckRun
  -- modifyMVar is like modifyMVar_ but the function returns a tuple of the new value
  -- and an arbitrary extra value, which in this case ends up telling whileM whether
  -- to keep looping.
  where updateAndCheckRun st = do
          now <- getCurrentTime
          let value' = integrate (func st) (value st) (time st) now
          evaluate value'                             -- avoid undesired laziness
          return (st { value = value', time  = now }, -- updated state
                  run st)                             -- whether to continue

integrate :: Fractional a => Func a -> a -> Time -> Time -> a
integrate f value t0 t1 = value + (f t0 + f t1)/2 * dt
  where dt = timeInterval t0 t1

-- Execute 'action' until it returns false.
whileM action = do b <- action; if b then whileM action else return ()
