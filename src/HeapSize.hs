{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- |
   Module      : HeapSize
   Copyright   : (c) Michail Pardalos
   License     : 3-Clause BSD-style
   Maintainer  : mpardalos@gmail.com

   Based on GHC.Datasize by Dennis Felsing
 -}
module HeapSize (
  recursiveSize,
  recursiveSizeNF,
  closureSize,
  Heapsize,
  runHeapsize
  )
  where

import Control.DeepSeq (NFData, force)
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Hashable
import qualified Data.HashTable.IO as HT
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)

import GHC.Exts hiding (closureSize#)
import GHC.Arr
import GHC.Exts.Heap hiding (size)
import qualified Data.Foldable as F

import System.Mem
import System.Mem.Weak
import Debug.Trace

foreign import prim "aToWordzh" aToWord# :: Any -> Word#
foreign import prim "unpackClosurePtrs" unpackClosurePtrs# :: Any -> Array# b
foreign import prim "closureSize" closureSize# :: Any -> Int#

----------------------------------------------------------------------------

-- | Get the *non-recursive* size of an closure in words
closureSize :: a -> IO Int
closureSize x = return (I# (closureSize# (unsafeCoerce# x)))

getClosures :: a -> IO (Array Int Box)
getClosures x = case unpackClosurePtrs# (unsafeCoerce# x) of
    pointers ->
      let nelems = I# (sizeofArray# pointers)
      in pure (fmap Box $ Array 0 (nelems - 1) nelems pointers)

--------------------------------------------------------------------------------

data HeapsizeState = HeapsizeState
  {
    -- | A mutable seen set
    closuresSeen :: HT.BasicHashTable HashableBox (),
    -- | A counter for the seen set
    seenSizeRef  :: IORef Int,
    -- | Did the GC run since the computation start?
    gcDetect     :: GcDetector
  }

-- | A one-shot device for detecting garbage collections
newtype GcDetector = GcDetector {gcSinceCreation :: IO Bool}

gcDetector :: IO GcDetector
gcDetector = do
  ref <- newIORef ()
  w <- mkWeakIORef ref (return ())
  return $ GcDetector $ isNothing <$> deRefWeak w

newtype Heapsize a = Heapsize
  { _unHeapsize :: ReaderT HeapsizeState (MaybeT IO) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadMask, MonadThrow)

initSize :: Int
initSize = 4000000

--   A garbage collection is performed before the size is calculated, because
--   the garbage collector would make heap walks difficult.
--   Returns `Nothing` if the count is interrupted by a garbage collection
runHeapsize :: Heapsize a -> IO (Maybe a)
runHeapsize (Heapsize comp) = do

  -- initialize the mutable state
  !closuresSeen <- HT.newSized initSize
  seenSizeRef   <- newIORef 0

  -- Perform a major GC
  performMajorGC

  -- Create a GC detector for the duration of the entire computation
  gcDetect <- gcDetector

  runMaybeT $ runReaderT comp HeapsizeState{..}

--------------------------------------------------------------------------------

-- | Calculate the recursive size of GHC objects in Bytes. Note that the actual
--   size in memory is calculated, so shared values are only counted once.
--
--   Call with
--   @
--    recursiveSize $! 2
--   @
--   to force evaluation to WHNF before calculating the size.
--
--   Call with
--   @
--    recursiveSize $!! \"foobar\"
--   @
--   ($!! from Control.DeepSeq) to force full evaluation before calculating the
--   size.
--
--   This function works very quickly on small data structures, but can be slow
--   on large and complex ones. If speed is an issue it's probably possible to
--   get the exact size of a small portion of the data structure and then
--   estimate the total size from that.
recursiveSize :: a -> Heapsize Int
recursiveSize x = Heapsize $ do
  HeapsizeState{..} <- ask
  accSizeRef <- liftIO $ newIORef 0
  let checkGC = gcSinceCreation gcDetect >>= \abort -> when abort $ throwIO Interrupted
  let
    go :: [ Box ] -> IO ()
    go [] = return ()
    go (b@(Box y) : rest) = do
      !seen <- isJust <$> HT.lookup closuresSeen (HashableBox b)

      next <- if seen then return [] else do
          -- always check that GC has not happened before deref pointers
          checkGC
          thisSize <- closureSize y
          checkGC
          next <- getClosures y

          HT.insert closuresSeen (HashableBox b) ()
          modifyIORef' accSizeRef (+ thisSize)
          modifyIORef' seenSizeRef succ
          return (F.toList next)
      go (next ++ rest)

  liftIO (go [asBox x]) `catch` \Interrupted -> do
    seen <- liftIO $ readIORef seenSizeRef
    liftIO $ traceIO ("SEEN: " <> show seen)
    mzero
  liftIO (readIORef accSizeRef)

data Interrupted = Interrupted deriving (Show, Typeable)
instance Exception Interrupted

-- | Calculate the recursive size of GHC objects in Bytes after calling
-- Control.DeepSeq.force on the data structure to force it into Normal Form.
-- Using this function requires that the data structure has an `NFData`
-- typeclass instance.
recursiveSizeNF :: NFData a => a -> Heapsize Int
recursiveSizeNF = recursiveSize . force

newtype HashableBox = HashableBox Box
    deriving newtype Show

-- | Pointer Equality
instance Eq HashableBox where
    (HashableBox (Box a1)) == (HashableBox (Box a2)) =
        W# (aToWord# a1) == W# (aToWord# a2)

-- | Pointer hash
instance Hashable HashableBox where
    hashWithSalt n (HashableBox (Box a)) = hashWithSalt n (W# (aToWord# a))
