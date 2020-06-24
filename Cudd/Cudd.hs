{-# LANGUAGE ForeignFunctionInterface, CPP, FlexibleContexts, RankNTypes #-}

module Cudd.Cudd (
    DdManager(),
    DdNode(),
    cuddInit,
    cuddInitOrder, 
    cuddDisabledGarbageCollection,
    cuddReadOne,
    cuddReadLogicZero,
    cuddBddIthVar,
    cuddZddIthVar, --ZDD stuff
    cuddZddITE,
    cuddZddVarsFromBddVars,
    cuddZddPortFromBdd,
    cuddZddPortToBdd,
    cuddZddReadOne,
    cuddZddReadOneIO,
    cuddPrintDdInfo,
    cuddZddIntersect,
    cuddZddUnion,
    cuddZddDiff,
    cuddZddSub0,
    cuddZddSub1,
    cuddZddChange,
    cuddZddProduct,
    cuddZddToDot,
    cuddZddReadZero,
    cuddZddVarFromBdd,
    cuddZddComplement,
    cuddDeref,
    cuddBddToDot, --other stuff
    cuddBddAnd,
    cuddBddOr,
    cuddBddNand,
    cuddBddNor,
    cuddBddXor,
    cuddBddXnor,
    cuddNot,
    cudd_cache_slots,
    cudd_unique_slots,
    cuddEval,
    cuddPrintMinterm,
    cuddAllSat,
    cuddOneSat,
    cuddOnePrime,
    cuddSupportIndex,
    cuddBddExistAbstract,
    cuddBddUnivAbstract,
    cuddBddIte,
    cuddBddPermute,
    cuddBddShift,
    cuddBddSwapVariables,
    cuddNodeReadIndex,
    cuddDagSize,
    cuddIndicesToCube,
    getManagerST,
    getSTManager,
    cuddBddLICompaction,
    cuddBddMinimize,
    cuddReadSize,
    cuddReadZddSize,
    cuddXeqy,
    cuddXgty,
    cuddBddInterval,
    cuddDisequality,
    cuddInequality,
    ddNodeToInt,
    cuddBddImp,
    cuddBddPickOneMinterm,
    cuddReadPerm,
    cuddReadInvPerm,
    cuddReadPerms,
    cuddReadInvPerms,
    cuddReadTree,
    cuddCountLeaves,
    cuddCountMinterm,
    cuddCountPath,
    cuddCountPathsToNonZero,
    cuddPrintDebug,
    cuddBddAndAbstract,
    cuddBddXorExistAbstract,
    cuddBddTransfer,
    cuddBddMakePrime,
    cuddBddConstrain,
    cuddBddRestrict,
    cuddBddSqueeze,
    SatBit(..),
    cuddLargestCube,
    cuddBddLeq,
    cuddDebugCheck,
    cuddCheckKeys,
    cuddPrintInfo
    ) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.IO.Unsafe
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad
import Data.List
import Data.Array hiding (indices)
import Control.Exception

import Cudd.ForeignHelpers
import Cudd.Internal
import Cudd.MTR
import Cudd.C
import Cudd.Common

cuddInit :: DdManager
cuddInit = DdManager $ unsafePerformIO $ c_cuddInit 0 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0

cuddDeref :: DdManager -> DdNode -> IO()
cuddDeref (DdManager d) (DdNode n) = withForeignPtr n $ \np -> do 
     newForeignPtrEnv derefZ d np
     return ()

cuddInitOrder :: [Int] -> DdManager
cuddInitOrder order = DdManager $ unsafePerformIO $ withArrayLen (map fromIntegral order) $ \size ptr -> do
    when (sort order /= [0..size-1]) (error "cuddInitOrder: order does not contain each variable once")
    m <- c_cuddInit (fromIntegral size) 0 (fromIntegral cudd_unique_slots) (fromIntegral cudd_cache_slots) 0
    res <- c_cuddShuffleHeap m ptr
    when (res /= 1) (error "shuffleHeap failed")
    return m

getManagerST :: STDdManager s u -> DdManager
getManagerST (STDdManager m) = DdManager m

getSTManager :: DdManager -> STDdManager s u
getSTManager (DdManager m) = STDdManager m

cuddReadOne :: DdManager -> DdNode
cuddReadOne (DdManager d) = DdNode $ unsafePerformIO $ do
    node <- c_cuddReadOneWithRef d
    newForeignPtrEnv deref d node

cuddReadLogicZero :: DdManager -> DdNode
cuddReadLogicZero (DdManager d) = DdNode $ unsafePerformIO $ do
    node <- c_cuddReadLogicZeroWithRef d
    newForeignPtrEnv deref d node

cuddBddIthVar :: DdManager -> Int -> DdNode
cuddBddIthVar (DdManager d) i = DdNode $ unsafePerformIO $ do
    node <- c_cuddBddIthVar d (fromIntegral i)
    newForeignPtr_ node

--Zdd stuff
cuddZddIthVar :: DdManager -> Int -> DdNode
cuddZddIthVar (DdManager d) i = DdNode $ unsafePerformIO $ do
    node <- c_cuddZddIthVar d (fromIntegral i)
    newForeignPtr_ node

cuddZddReadOne :: DdManager -> DdNode
cuddZddReadOne (DdManager d) = DdNode $ unsafePerformIO $ do
    node <- c_cuddZddReadOneWithRef d
    newForeignPtrEnv derefZ d node

cuddZddReadOneIO :: DdManager -> IO(DdNode)
cuddZddReadOneIO (DdManager d) = do 
    return $ DdNode $ unsafePerformIO $ do
        node <- c_cuddZddReadOneWithRef d
        newForeignPtrEnv derefZ d node

cuddZddReadZero :: DdManager -> DdNode
cuddZddReadZero (DdManager d) = DdNode $ unsafePerformIO $ do
    node <- c_cuddZddReadZero d
    newForeignPtrEnv derefZ d node

cuddZddVarFromBdd :: DdManager -> IO()
cuddZddVarFromBdd (DdManager d) = do
    c_cuddZddVarsFromBddVars d (fromIntegral 1)
    return ()

cuddZddPortFromBdd :: DdManager -> DdNode -> DdNode
cuddZddPortFromBdd (DdManager d) (DdNode n) = DdNode $ unsafePerformIO $ withForeignPtr n $ \np -> do
    node <- c_cuddZddPortFromBdd d np
    newForeignPtrEnv derefZ d node

cuddZddPortToBdd :: DdManager -> DdNode -> DdNode
cuddZddPortToBdd (DdManager d) (DdNode n) = DdNode $ unsafePerformIO $ withForeignPtr n $ \np -> do
    node <- c_cuddZddPortToBdd d np
    newForeignPtrEnv deref d node

cuddZddVarsFromBddVars :: DdManager -> Int -> IO()
cuddZddVarsFromBddVars (DdManager d) i = do
    c_cuddZddVarsFromBddVars d (fromIntegral i)

cuddPrintDdInfo :: DdManager -> DdNode -> IO()
cuddPrintDdInfo (DdManager d) (DdNode n) = 
    withForeignPtr n $ \np -> do 
    c_cuddPrintDdInfo d np 0 3 

cuddZddITE :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode
cuddZddITE = cuddArg3Z c_cuddZddITE

cuddZddIntersect :: DdManager -> DdNode -> DdNode -> DdNode
cuddZddIntersect = cuddArg2Z c_cuddZddIntersect

cuddZddUnion :: DdManager -> DdNode -> DdNode -> DdNode
cuddZddUnion = cuddArg2Z c_cuddZddUnion

cuddZddProduct :: DdManager -> DdNode -> DdNode -> DdNode
cuddZddProduct = cuddArg2Z c_cuddZddProduct

cuddZddDiff :: DdManager -> DdNode -> DdNode -> DdNode
cuddZddDiff = cuddArg2Z c_cuddZddDiff

cuddZddComplement :: DdManager -> DdNode -> DdNode
cuddZddComplement (DdManager d) (DdNode n) = DdNode $ unsafePerformIO $ withForeignPtr n $ \np -> do
    node <- c_cuddZddPortFromBdd d np
    newForeignPtrEnv derefZ d node

cuddZddToDot :: DdManager -> DdNode -> String -> IO()
cuddZddToDot (DdManager m) (DdNode n) file = 
    withForeignPtr n $ \np -> 
        c_cuddZddToDot m np f where
            f = unsafePerformIO $ newCString file

cuddZddSub0 :: DdManager -> DdNode -> Int -> DdNode
cuddZddSub0 (DdManager m) (DdNode n) i =
    DdNode $ unsafePerformIO $ withForeignPtr n $ \np -> do 
        node <- c_cuddZddSub0 m np (fromIntegral i)
        newForeignPtrEnv derefZ m node

cuddZddSub1 :: DdManager -> DdNode -> Int -> DdNode
cuddZddSub1 (DdManager m) (DdNode n) i =
    DdNode $ unsafePerformIO $ withForeignPtr n $ \np -> do 
        node <- c_cuddZddSub1 m np (fromIntegral i)
        newForeignPtrEnv derefZ m node

cuddZddChange :: DdManager -> DdNode -> Int -> DdNode
cuddZddChange (DdManager m) (DdNode n) i =
    DdNode $ unsafePerformIO $ withForeignPtr n $ \np -> do 
        node <- c_cuddZddChange m np (fromIntegral i)
        newForeignPtrEnv derefZ m node

--bool = 0, returns a zdd where the variable is false
{-cuddZddRestrict :: DdManager -> DdNode -> Int -> Bool -> IO(DdNode) 
cuddZddRestrict m n i b = 
    if b then cuddZddSub1 m n i else cuddZddSub0 m n i-}

cuddBddToDot :: DdManager -> DdNode -> String -> IO()
cuddBddToDot (DdManager m) (DdNode n) file = 
    withForeignPtr n $ \np -> do 
        c_cuddBddToDot m np f where
            f = unsafePerformIO $ newCString file

cuddDisabledGarbageCollection :: DdManager -> IO()
cuddDisabledGarbageCollection (DdManager m) = c_cuddDisabledGarbageCollection m
--other stuff

cuddArg1 :: (Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode
cuddArg1 f (DdManager m) (DdNode x) = DdNode $ unsafePerformIO $
    withForeignPtr x $ \xp -> do
    node <- f m xp
    newForeignPtrEnv deref m node

cuddArg2 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode -> DdNode
cuddArg2 f (DdManager m) (DdNode l) (DdNode r) = DdNode $ unsafePerformIO $
    withForeignPtr l $ \lp ->
    withForeignPtr r $ \rp -> do
    node <- f m lp rp
    newForeignPtrEnv deref m node

cuddArg3 :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode-> DdNode -> DdNode
cuddArg3 f (DdManager m) (DdNode l) (DdNode r) (DdNode x) = DdNode $ unsafePerformIO $
    withForeignPtr l $ \lp ->
    withForeignPtr r $ \rp ->
    withForeignPtr x $ \xp -> do
    node <- f m lp rp xp
    newForeignPtrEnv deref m node

cuddArg1Z :: (Ptr CDdManager -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode
cuddArg1Z f (DdManager m) (DdNode x) = DdNode $ unsafePerformIO $
    withForeignPtr x $ \xp -> do
    node <- f m xp
    newForeignPtrEnv derefZ m node

cuddArg2Z :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode -> DdNode
cuddArg2Z f (DdManager m) (DdNode l) (DdNode r) = DdNode $ unsafePerformIO $
    withForeignPtr l $ \lp ->
    withForeignPtr r $ \rp -> do
    node <- f m lp rp
    newForeignPtrEnv derefZ m node

cuddArg3Z :: (Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CDdNode -> IO (Ptr CDdNode)) -> DdManager -> DdNode -> DdNode-> DdNode -> DdNode
cuddArg3Z f (DdManager m) (DdNode l) (DdNode r) (DdNode x) = DdNode $ unsafePerformIO $
    withForeignPtr l $ \lp ->
    withForeignPtr r $ \rp ->
    withForeignPtr x $ \xp -> do
    node <- f m lp rp xp
    newForeignPtrEnv derefZ m node

cuddBddAnd :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddAnd = cuddArg2 c_cuddBddAnd

cuddBddOr :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddOr = cuddArg2 c_cuddBddOr

cuddBddNand :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddNand = cuddArg2 c_cuddBddNand

cuddBddNor :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddNor = cuddArg2 c_cuddBddNor

cuddBddXor :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddXor = cuddArg2 c_cuddBddXor

cuddBddXnor :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddXnor = cuddArg2 c_cuddBddXnor

cuddNot :: DdManager -> DdNode -> DdNode
cuddNot = cuddArg1 (const c_cuddNot)

foreign import ccall safe "cuddwrap.h wrappedCuddDumpDot"
    c_cuddDumpDot :: Ptr CDdManager -> Ptr CDdNode -> CString -> IO ()

cuddDumpDot :: DdManager -> DdNode -> String -> IO ()
cuddDumpDot (DdManager m) (DdNode n) s  =
    withForeignPtr n $ \np ->
        withCAString s $ \str ->
            c_cuddDumpDot m np str

foreign import ccall safe "cuddwrap.h wrappedCuddIsComplement"
    c_cuddIsComplement :: Ptr CDdNode -> CInt

cuddEval :: DdManager -> DdNode -> [Int] -> Bool
cuddEval (DdManager m) (DdNode n) a = unsafePerformIO $ do
    res <- withArray (map fromIntegral a) $ \app ->
        withForeignPtr n $ \np ->
            c_cuddEval m np app
    return $ (==0) $ c_cuddIsComplement res

cuddPrintMinterm :: DdManager -> DdNode -> IO ()
cuddPrintMinterm (DdManager m) (DdNode n) =
    withForeignPtr n $ c_cuddPrintMinterm m

foreign import ccall safe "cuddwrap.h allSat"
    c_allSat :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> Ptr CInt -> IO (Ptr (Ptr CInt))

foreign import ccall safe "cuddwrap.h oneSat"
    c_oneSat :: Ptr CDdManager -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CInt)

cuddAllSat :: DdManager -> DdNode -> [[SatBit]]
cuddAllSat (DdManager m) (DdNode n) = unsafePerformIO $
    alloca $ \nvarsptr ->
    alloca $ \ntermsptr ->
    withForeignPtr n $ \np -> do
    res <- c_allSat m np ntermsptr nvarsptr
    nterms <- fromIntegral <$> peek ntermsptr
    res2 <- peekArray nterms res
    nvars <- fromIntegral <$> peek nvarsptr
    res3 <- mapM (peekArray nvars) res2
    return $ map (map (toSatBit . fromIntegral)) res3

cuddOneSat :: DdManager -> DdNode -> Maybe [SatBit]
cuddOneSat (DdManager m) (DdNode n) = unsafePerformIO $
    alloca $ \nvarsptr ->
    withForeignPtr n $ \np -> do
    res <- c_oneSat m np nvarsptr
    if res==nullPtr then return Nothing else do
        nvars <- fromIntegral <$> peek nvarsptr
        res2 <- peekArray nvars res
        return $ Just $ map (toSatBit . fromIntegral) res2

foreign import ccall safe "cuddwrap.h onePrime"
    c_onePrime :: Ptr CDdManager -> Ptr CDdNode -> Ptr CDdNode -> Ptr CInt -> IO (Ptr CInt)

cuddOnePrime :: DdManager -> DdNode -> DdNode -> Maybe [Int]
cuddOnePrime (DdManager m) (DdNode l) (DdNode u) = unsafePerformIO $
    alloca $ \nvarsptr ->
    withForeignPtr l $ \lp ->
    withForeignPtr u $ \up -> do
    res <- c_onePrime m lp up nvarsptr
    if res==nullPtr then return Nothing else do
        nvars <- fromIntegral <$> peek nvarsptr
        res2 <- peekArray nvars res
        return $ Just $ map fromIntegral res2

cuddReadSize :: DdManager -> Int
cuddReadSize (DdManager m) = fromIntegral $ unsafePerformIO $ c_cuddReadSize m

cuddReadZddSize :: DdManager -> Int
cuddReadZddSize (DdManager m) = fromIntegral $ unsafePerformIO $ c_cuddReadZddSize m

cuddSupportIndex :: DdManager -> DdNode -> [Bool]
cuddSupportIndex (DdManager m) (DdNode n) = unsafePerformIO $
    withForeignPtr n $ \np -> do
        res <- c_cuddSupportIndex m np
        size <- c_cuddReadSize m
        res2 <- peekArray (fromIntegral size) res
        return $ map toBool res2

cuddBddExistAbstract :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddExistAbstract = cuddArg2 c_cuddBddExistAbstract

cuddBddUnivAbstract :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddUnivAbstract = cuddArg2 c_cuddBddUnivAbstract

cuddBddIte :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode
cuddBddIte = cuddArg3 c_cuddBddIte

cuddBddSwapVariables :: DdManager -> DdNode -> [DdNode] -> [DdNode] -> DdNode
cuddBddSwapVariables (DdManager m) (DdNode d) s1 s2 = DdNode $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    withForeignArrayPtrLen (map unDdNode s1) $ \s1p s1ps ->
    withForeignArrayPtrLen (map unDdNode s2) $ \s2p s2ps -> do
    when (s1p /= s2p) (error "cuddBddSwapVariables: variable lists have different sizes")
    node <- c_cuddBddSwapVariables m dp s1ps s2ps (fromIntegral s1p)
    newForeignPtrEnv deref m node

cuddBddPermute :: DdManager -> DdNode -> [Int] -> DdNode
cuddBddPermute (DdManager m) (DdNode d) indexes = DdNode $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    withArray (map fromIntegral indexes) $ \ip -> do
    node <- c_cuddBddPermute m dp ip
    newForeignPtrEnv deref m node

makePermutArray :: Int -> [Int] -> [Int] -> [Int]
makePermutArray size x y = elems $ accumArray (flip const) 0 (0, size-1) ascList
    where
    ascList = [(i, i) | i <- [0..size-1]] ++ zip x y

cuddBddShift :: DdManager -> DdNode -> [Int] -> [Int] -> DdNode
cuddBddShift (DdManager m) (DdNode d) from to = DdNode $ unsafePerformIO $
    withForeignPtr d $ \dp0 -> do
    dp <- evaluate dp0
    size <- c_cuddReadSize m
    let perm = makePermutArray (fromIntegral size) from to
    withArray (map fromIntegral perm) $ \pp -> do
        node <- c_cuddBddPermute m dp pp
        newForeignPtrEnv deref m node

cuddXgty :: DdManager -> [DdNode] -> [DdNode] -> DdNode
cuddXgty (DdManager m) x y = DdNode $ unsafePerformIO $
    withForeignArrayPtrLen (map unDdNode x) $ \xl xp ->
    withForeignArrayPtrLen (map unDdNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXgty: variable lists have different sizes")
    node <- c_cuddXgty m (fromIntegral xl) nullPtr xp yp
    newForeignPtrEnv deref m node

cuddXeqy :: DdManager -> [DdNode] -> [DdNode] -> DdNode
cuddXeqy (DdManager m) x y = DdNode $ unsafePerformIO $
    withForeignArrayPtrLen (map unDdNode x) $ \xl xp ->
    withForeignArrayPtrLen (map unDdNode y) $ \yl yp -> do
    when (xl /= yl) (error "cuddXeqy: variable lists have different sizes")
    node <- c_cuddXeqy m (fromIntegral xl) xp yp
    newForeignPtrEnv deref m node

cuddInequality :: DdManager -> Int -> Int -> [DdNode] -> [DdNode] -> DdNode
cuddInequality (DdManager m) n c x y = DdNode $ unsafePerformIO $
    withForeignArrayPtr (map unDdNode x) $ \xp ->
    withForeignArrayPtr (map unDdNode y) $ \yp -> do
    node <- c_cuddInequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

cuddDisequality :: DdManager -> Int -> Int -> [DdNode] -> [DdNode] -> DdNode
cuddDisequality (DdManager m) n c x y = DdNode $ unsafePerformIO $
    withForeignArrayPtr (map unDdNode x) $ \xp ->
    withForeignArrayPtr (map unDdNode y) $ \yp -> do
    node <- c_cuddDisequality m (fromIntegral n) (fromIntegral c) xp yp
    newForeignPtrEnv deref m node

cuddBddInterval :: DdManager -> [DdNode] -> Int -> Int -> DdNode
cuddBddInterval (DdManager m) vararr lower upper =  DdNode $ unsafePerformIO $
    withForeignArrayPtrLen (map unDdNode vararr) $ \sz vp -> do
    node <- c_cuddBddInterval m (fromIntegral sz) vp (fromIntegral lower) (fromIntegral upper)
    newForeignPtrEnv deref m node

cuddNodeReadIndex :: DdNode -> Int
cuddNodeReadIndex (DdNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddNodeReadIndex

cuddDagSize :: Num b => DdNode -> b
cuddDagSize (DdNode d) = fromIntegral $ unsafePerformIO $ withForeignPtr d c_cuddDagSize

cuddIndicesToCube :: DdManager -> [Int] -> DdNode
cuddIndicesToCube (DdManager m) indices = DdNode $ unsafePerformIO $
    withArrayLen (map fromIntegral indices) $ \size ip -> do
    node <- c_cuddIndicesToCube m ip (fromIntegral size)
    newForeignPtrEnv deref m node

cuddBddLICompaction :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddLICompaction = cuddArg2 c_cuddBddLICompaction

cuddBddMinimize :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddMinimize = cuddArg2 c_cuddBddMinimize

--Bdd implication
cuddBddImp :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddImp m l = cuddBddOr m (cuddNot m l)

cuddBddPickOneMinterm :: DdManager -> DdNode -> [DdNode] -> Maybe DdNode
cuddBddPickOneMinterm (DdManager m) (DdNode d) vars = unsafePerformIO $
    withForeignPtr d $ \dp ->
    withForeignArrayPtrLen (map unDdNode vars) $ \vs vp -> do
    node <- c_cuddBddPickOneMinterm m dp vp (fromIntegral vs)
    if node == nullPtr then return Nothing else do
        nd <- newForeignPtrEnv deref m node
        return $ Just $ DdNode nd

foreign import ccall safe "cudd.h Cudd_PrintInfo"
    c_cuddPrintInfo :: Ptr CDdManager -> Ptr CFile -> IO CInt

cuddPrintInfo :: DdManager -> Ptr CFile -> IO Int
cuddPrintInfo (DdManager m) cf = fromIntegral <$> c_cuddPrintInfo m cf

cuddReadPerm :: DdManager -> Int -> Int
cuddReadPerm (DdManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadPerm m (fromIntegral i)

cuddReadInvPerm :: DdManager -> Int -> Int
cuddReadInvPerm (DdManager m) i = fromIntegral $ unsafePerformIO $ c_cuddReadInvPerm m (fromIntegral i)

cuddReadPerms :: DdManager -> [Int]
cuddReadPerms m = map (cuddReadPerm m) [0..(cuddReadSize m - 1)]

cuddReadInvPerms :: DdManager -> [Int]
cuddReadInvPerms m = map (cuddReadInvPerm m) [0..(cuddReadSize m -1)]

cuddReadTree :: DdManager -> IO (Ptr CMtrNode)
cuddReadTree (DdManager m) = c_cuddReadTree m

cuddCountLeaves :: DdNode -> Int
cuddCountLeaves (DdNode d) = fromIntegral $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountLeaves dp

cuddCountMinterm :: DdManager -> DdNode -> Int -> Double
cuddCountMinterm (DdManager m) (DdNode d) n = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountMinterm m dp (fromIntegral n)

cuddCountPathsToNonZero :: DdNode -> Double
cuddCountPathsToNonZero (DdNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountPathsToNonZero dp

cuddCountPath :: DdNode -> Double
cuddCountPath (DdNode d) = realToFrac $ unsafePerformIO $
    withForeignPtr d $ \dp ->
    c_cuddCountPath dp

foreign import ccall safe "cudd.h Cudd_PrintDebug"
    c_cuddPrintDebug :: Ptr CDdManager -> Ptr CDdNode -> CInt -> CInt -> IO CInt

cuddPrintDebug :: DdManager -> DdNode -> Int -> Int -> IO Int
cuddPrintDebug (DdManager m) (DdNode d) n pr = fmap fromIntegral $
    withForeignPtr d $ \dp ->
    c_cuddPrintDebug m dp (fromIntegral n) (fromIntegral pr)

cuddBddAndAbstract :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode
cuddBddAndAbstract = cuddArg3 c_cuddBddAndAbstract

cuddBddXorExistAbstract :: DdManager -> DdNode -> DdNode -> DdNode -> DdNode
cuddBddXorExistAbstract = cuddArg3 c_cuddBddXorExistAbstract

cuddBddTransfer :: DdManager -> DdManager -> DdNode -> DdNode
cuddBddTransfer (DdManager m1) (DdManager m2) (DdNode x) = DdNode $ unsafePerformIO $
    withForeignPtr x $ \xp -> do
        node <- c_cuddBddTransfer m1 m2 xp
        newForeignPtrEnv deref m2 node

cuddBddMakePrime :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddMakePrime = cuddArg2 c_cuddBddMakePrime

cuddBddConstrain :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddConstrain = cuddArg2 c_cuddBddConstrain

cuddBddRestrict :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddRestrict = cuddArg2 c_cuddBddRestrict

cuddBddSqueeze :: DdManager -> DdNode -> DdNode -> DdNode
cuddBddSqueeze = cuddArg2 c_cuddBddSqueeze

cuddLargestCube :: DdManager -> DdNode -> (Int, DdNode)
cuddLargestCube (DdManager m) (DdNode n) = unsafePerformIO $
    alloca $ \lp ->
    withForeignPtr n $ \np -> do
    node <- c_cuddLargestCube m np lp
    res <- newForeignPtrEnv deref m node
    l <- peek lp
    return (fromIntegral l, DdNode res)

cuddBddLeq :: DdManager -> DdNode -> DdNode -> Bool
cuddBddLeq (DdManager m) (DdNode l) (DdNode r) = (==1) $ unsafePerformIO $
    withForeignPtr l $ \lp ->
    withForeignPtr r $ \rp ->
    c_cuddBddLeq m lp rp

cuddCheckKeys :: DdManager -> ST s Int
cuddCheckKeys (DdManager m) = fmap fromIntegral $ unsafeIOToST $ c_cuddCheckKeys m

cuddDebugCheck :: DdManager -> ST s Int
cuddDebugCheck (DdManager m) = fmap fromIntegral $ unsafeIOToST $ c_cuddDebugCheck m

ddNodeToInt :: Integral i => DdNode -> i
ddNodeToInt = fromIntegral . ptrToIntPtr . unsafeForeignPtrToPtr . unDdNode
