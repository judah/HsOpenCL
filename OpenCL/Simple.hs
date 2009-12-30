module OpenCL.Simple(
            SimpleProgram(..),
            DeviceType(..),
            newSimpleProgram,
            getKernelFunc,
            KernelArg(),
            KernelFunc(),
            )
             where

import OpenCL hiding (KernelArg)
import Data.Array.CArray
import Data.Array.IOCArray
import Data.Array.CArray.Base(unsafeFreezeIOCArray, IOCArray(..))
import System.IO
import Control.Exception
import Foreign
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe

-- TODO: this isn't really necessary, since
-- all of the types can ask for their context/id/etc.
data SimpleProgram = SimpleProgram {
                        simpleID :: DeviceID
                        , simpleCxt :: Context
                        , simpleQueue :: CommandQueue
                        , simpleProgram :: Program
                        }

-- TODO: should be bytestring.
newSimpleProgram :: DeviceType -> [ByteString] -> IO SimpleProgram
newSimpleProgram devType sources = do
    devID <- getDeviceID devType
    cxt <- createContext [devID]
    queue <- createCommandQueue cxt devID []
    prog <- buildSimpleProgram devID cxt sources
    return $ SimpleProgram devID cxt queue prog

buildSimpleProgram :: DeviceID -> Context -> [ByteString] -> IO Program
buildSimpleProgram did cxt sources = do
    prog <- createProgramWithSource cxt sources
    handle (\(e::CLError) -> do
                hPutStrLn stderr $ "Error building program: " ++ show e
                log <- getBuildLog prog did
                B.putStrLn log
                throw e)
        $ buildProgram prog ""
    return prog


------------

-- TODO: we only do blocking read/writes

-- TODO: should use CSize.
type Param = (SomeArg,Maybe Int)

class KernelFunc f where
    applyKFunc :: Kernel -> SimpleProgram -> [Param] -> f
    liftWith :: (forall c . (b -> IO c) -> IO c) -> (b -> f) -> f

getKernelFunc :: KernelFunc f => SimpleProgram -> String -> IO f
getKernelFunc prog text = do
    kernel <- createKernel (simpleProgram prog) text
    return $ applyKFunc kernel prog []

instance KernelFunc (IO ()) where
    applyKFunc kernel prog ps = runKernel kernel prog (reverse ps)
                                    >> return ()
    liftWith = id

runKernel :: Kernel -> SimpleProgram -> [Param] -> IO Event
runKernel kernel prog params = case commonSize (map snd params) of
    Nothing -> enqueueTask (simpleQueue prog) kernel []
    Just size -> do
        setKernelArgs kernel (map fst params)
        enqueueNDRangeKernel (simpleQueue prog) kernel size
                    Nothing []

-- Nothing means we should use enqueueTask.
commonSize :: [Maybe Int] -> Maybe Int
commonSize xs = case catMaybes xs of
                    [] -> Nothing
                    (x:xs)
                        | any (/=x) xs
                            -> error "Kernel arguments are not all the same size."
                        | otherwise -> Just x


instance (Storable e) =>  KernelFunc (IO (CArray Int e)) where
    liftWith = id
    applyKFunc kernel prog ps = case commonSize (map snd ps) of
        Nothing -> error "Can't guess size of return arrays"
        Just size -> do
            a <- newArray_ (0,size-1)
            withIOCArray a $ \p -> do
            bracket
                (createBuffer (simpleCxt prog) MemWriteOnly NoHostPtr size)
                (\m -> do
                        enqueueReadBuffer (simpleQueue prog) m Blocking
                                        0 size p []
                        releaseMemObject m)
                $ \m -> (applyKFunc kernel prog ((SomeArg m, Just size):ps)
                                    :: IO ())
            unsafeFreezeIOCArray a


class KernelArg a where
    withArg :: SimpleProgram -> a -> (Param->IO b) -> IO b

instance (KernelArg a, KernelFunc f) => KernelFunc (a -> f) where
    applyKFunc k prog ps = \a -> liftWith (withArg prog a)
                        $ \p -> applyKFunc k prog (p:ps)
    liftWith with f = \x -> liftWith with (\y -> f y x)

instance Storable a => KernelArg (Scalar a) where
    withArg p x f = f (SomeArg x, Nothing)

instance Storable a => KernelArg (Local a) where
   withArg _ l@(Local n) f = f (SomeArg l, Just n)

instance Storable a => KernelArg (Buffer a) where
    withArg _ m f = f (SomeArg m, Just $ memSize m `div` sizeOf (undefined :: a))
                        

instance (Ix i, Storable e) => KernelArg (CArray i e) where
    withArg prog a = \f -> withCArray a $ \p -> do
                            let size = rangeSize (bounds a)
                            bracket
                                (createBuffer (simpleCxt prog) MemReadOnly
                                    (CopyHostPtr p) size)
                                releaseMemObject
                                $ \m -> f (SomeArg m, Just size)

instance (Ix i, Storable e) => KernelArg (IOCArray i e) where
    withArg prog a = \f ->
        withIOCArray a $ \p -> do
            size <- rangeSize <$> getBounds a
            bracket
                (createBuffer (simpleCxt prog) MemReadWrite
                        (CopyHostPtr p) size)
                (\m -> do
                    enqueueReadBuffer (simpleQueue prog) m Blocking
                            0 size p []
                    releaseMemObject m)
                $ \m -> f (SomeArg m, Just size)


