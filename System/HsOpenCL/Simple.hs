module System.HsOpenCL.Simple(
            SimpleProgram(..),
            DeviceType(..),
            newSimpleProgram,
            getKernelFunc,
            KernelArg(),
            KernelFunc(),
            )
             where

import System.HsOpenCL hiding (KernelArg, KernelFunc)
import Data.Array.CArray
import Data.Array.IOCArray
import Data.Array.CArray.Base(unsafeFreezeIOCArray, IOCArray(..))
import System.IO
import Control.Exception
import Foreign
import Control.Applicative
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
    applyKFunc kernel prog ps = runKernel' kernel prog (reverse ps)
                                    >> return ()
    liftWith = id

runKernel' :: Kernel -> SimpleProgram -> [Param] -> IO ()
runKernel' kernel prog params = do
    setKernelArgs kernel (map fst params)
    waitForCommand (simpleQueue prog)
        $ case commonSize (map snd params) of
            Nothing -> ndRangeKernel kernel () Nothing
            Just size -> ndRangeKernel kernel size Nothing

-- Nothing means we should use enqueueTask.
commonSize :: [Maybe Int] -> Maybe Int
commonSize xs = case catMaybes xs of
                    [] -> Nothing
                    (y:ys)
                        | any (/=y) ys
                            -> error "Kernel arguments are not all the same size."
                        | otherwise -> Just y


instance (Storable e) =>  KernelFunc (IO (CArray Int e)) where
    liftWith = id
    applyKFunc kernel prog ps = case commonSize (map snd ps) of
        Nothing -> error "Can't guess size of return arrays"
        Just size -> do
            a <- newArray_ (0,size-1)
            withIOCArray a $ \p -> do
            bracketBuffer prog MemWriteOnly NoHostPtr size
                (\m -> enqueue_ (simpleQueue prog)
                            $ readBuffer m Blocking 0 size p)
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
                            bracketBuffer prog MemReadOnly (CopyHostPtr p) size
                                return
                                $ \m -> f (SomeArg m, Just size)

instance (Ix i, Storable e) => KernelArg (IOCArray i e) where
    withArg prog a = \f ->
        withIOCArray a $ \p -> do
            size <- rangeSize <$> getBounds a
            bracketBuffer prog MemReadWrite (CopyHostPtr p) size
                (\m -> enqueue_ (simpleQueue prog)
                        $ readBuffer m Blocking 0 size p)
                $ \m -> f (SomeArg m, Just size)


bracketBuffer :: Storable e => SimpleProgram -> MemAccessFlag -> MemInitFlag e -> Int
                    -> (Buffer e -> IO a) -> (Buffer e -> IO b) -> IO b
bracketBuffer prog access init size end f
    = withBuffer (simpleCxt prog) access init size
        $ \m -> do {x <- f m; end m; return x}
