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
import Control.Exception (throwIO, evaluate)
import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.HashTable.IO as HT
import Data.Maybe (isJust, isNothing)

import GHC.Exts hiding (closureSize#)
import GHC.Arr
import GHC.Exts.Heap hiding (size)
import qualified Data.HashSet as H
import Data.IORef
import Data.Hashable

import Control.Monad

import System.Mem
import System.Mem.Weak
import Control.Monad.IO.Class
import Data.Typeable (Typeable)

foreign import prim "aToWordzh" aToWord# :: Any -> Word#
foreign import prim "unpackClosurePtrs" unpackClosurePtrs# :: Any -> Array# b
foreign import prim "closureSize" closureSize# :: Any -> Int#

----------------------------------------------------------------------------
newtype GcDetector = GcDetector {gcSinceCreation :: IO Bool}

gcDetector :: IO GcDetector
gcDetector = do
  ref <- newIORef ()
  w <- mkWeakIORef ref (return ())
  return $ GcDetector $ isNothing <$> deRefWeak w

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
    accSizeRef   :: IORef Int,
    closuresSeen :: HT.BasicHashTable HashableBox ()
  }

newtype Heapsize a = Heapsize
  { _unHeapsize :: ReaderT (HeapsizeState, GcDetector) (MaybeT IO) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadCatch, MonadMask, MonadThrow)

initSize = 100000000

--   A garbage collection is performed before the size is calculated, because
--   the garbage collector would make heap walks difficult.
runHeapsize :: Heapsize a -> IO (Maybe a)
runHeapsize (Heapsize comp) = do
  stateRef <- newIORef 0
  !ht <- HT.newSized initSize
  performGC
  gcDetect <- gcDetector
  runMaybeT $ runReaderT comp (HeapsizeState stateRef ht, gcDetect)

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
--   Returns `Nothing` if the count is interrupted by a garbage collection
recursiveSize :: a -> Heapsize Int
recursiveSize x = Heapsize $ do
  (HeapsizeState{..}, gcDetect) <- ask
  let checkGC = gcSinceCreation gcDetect
  let
    go :: Box -> IO ()
    go b@(Box y) = do
      !seen <- isJust <$> HT.lookupIndex closuresSeen (HashableBox b)

      unless seen $ do

        let guardGC k = checkGC >>= \abort -> if abort then throwIO Interrupted else k

        guardGC $ closureSize y >>= \thisSize ->
          guardGC $ getClosures y >>= \next -> do
            modifyIORef accSizeRef (+ thisSize)
            HT.insert closuresSeen (HashableBox b) ()

            mapM_ go next

  liftIO (go (asBox x)) `catch` \Interrupted -> mzero
  liftIO (readIORef accSizeRef)

data Interrupted = Interrupted deriving (Show, Typeable)
instance Exception Interrupted

-- | Calculate the recursive size of GHC objects in Bytes after calling
-- Control.DeepSeq.force on the data structure to force it into Normal Form.
-- Using this function requires that the data structure has an `NFData`
-- typeclass instance.
-- Returns `Nothing` if the count is interrupted by a garbage collection
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
