{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, CPP #-}
module Cudd.Internal (
    DdManager(..),
    STDdManager(..),
    DdNode(..),
    DDNode(..),
    deref,
    derefZ,
    newNodeFP,
    cuddQuit,
    cuddLiveNodes,
    cudd_unique_slots,
    cudd_cache_slots
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import Cudd.C

#include <stdio.h>
#include "cudd.h"

cudd_unique_slots :: Int
cudd_unique_slots = #const CUDD_UNIQUE_SLOTS

cudd_cache_slots :: Int
cudd_cache_slots = #const CUDD_CACHE_SLOTS

newtype DdManager = DdManager (Ptr CDdManager) deriving (Eq, Ord, Show)

newtype STDdManager s u = STDdManager {unSTDdManager :: Ptr CDdManager}

newtype DdNode = DdNode {unDdNode :: ForeignPtr CDdNode} deriving (Ord, Eq, Show)

newtype DDNode s u = DDNode {unDDNode :: Ptr CDdNode} deriving (Ord, Eq, Show)

-- | Finalizer for BDD nodes: Cudd_IterDerefBdd plus manager lifetime bookkeeping.
deref :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())
deref = c_hsCuddDerefBddPtr

-- | Finalizer for ZDD nodes: Cudd_RecursiveDerefZdd plus manager lifetime bookkeeping.
derefZ :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())
derefZ = c_hsCuddDerefZddPtr

foreign import ccall unsafe "&hsCuddDerefBdd"
    c_hsCuddDerefBddPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall unsafe "&hsCuddDerefZdd"
    c_hsCuddDerefZddPtr :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ())

foreign import ccall unsafe "hsCuddIncLive"
    c_hsCuddIncLive :: Ptr CDdManager -> IO ()

foreign import ccall unsafe "hsCuddQuit"
    c_hsCuddQuit :: Ptr CDdManager -> IO ()

foreign import ccall unsafe "hsCuddLiveNodes"
    c_hsCuddLiveNodes :: Ptr CDdManager -> IO CLong

-- | Wrap an already referenced node in a ForeignPtr whose finalizer
-- (one of 'deref' or 'derefZ') releases that reference.
-- The finalizer must be 'deref' or 'derefZ', because those also do the
-- bookkeeping needed by 'cuddQuit'.
newNodeFP :: FunPtr (Ptr CDdManager -> Ptr CDdNode -> IO ()) -> Ptr CDdManager -> Ptr CDdNode -> IO (ForeignPtr CDdNode)
newNodeFP fin m node = do
    c_hsCuddIncLive m
    newForeignPtrEnv fin m node

-- | Free a manager. Nodes that are still referenced from Haskell keep it alive:
-- the actual Cudd_Quit happens as soon as the last of their finalizers has run.
-- After calling this, the manager and all its nodes must not be used anymore.
cuddQuit :: DdManager -> IO ()
cuddQuit (DdManager m) = c_hsCuddQuit m

-- | Number of nodes of this manager that are still owned by Haskell
-- (i.e. have a pending finalizer), or -1 if the manager has been freed.
-- Mainly useful for debugging leaks.
cuddLiveNodes :: DdManager -> IO Int
cuddLiveNodes (DdManager m) = fromIntegral <$> c_hsCuddLiveNodes m
