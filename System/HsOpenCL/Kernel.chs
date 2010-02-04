module System.HsOpenCL.Kernel(
                -- * Kernels
                Kernel,
                createKernel,
                createKernelsInProgram,
                -- * Running kernels
                runKernel,
                NDRange,
                KernelFunc,
                KernelArg(),
                -- ** KernelArg instances
                Scalar(..),
                Local(..),
                -- * Queries
                kernelFunctionName,
                kernelNumArgs,
                kernelContext,
                kernelProgram,
                -- ** Work group queries
                kernelWorkGroupSize,
                kernelCompileWorkGroupSize,
                getKernelLocalMemSize,
                ) where

#include <OpenCL/OpenCL.h>
import System.HsOpenCL.Internal.Types
import System.HsOpenCL.Internal.C2HS
import System.HsOpenCL.Error
import System.HsOpenCL.Platform(Size,ULong)
import System.HsOpenCL.CommandQueue
import Control.Monad

{#fun clCreateKernel
  { withProgram* `Program'
  , `String'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Kernel' newKernel*
#}

createKernel :: MonadIO m => Program -> String -> m Kernel
createKernel p name = liftIO $ clCreateKernel p name

newKernel = newData Kernel clReleaseKernel
foreign import ccall "&" clReleaseKernel :: Releaser Kernel_

{#fun clCreateKernelsInProgram
  { withProgram* `Program'
  , `Int'
  , castPtr `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}


createKernelsInProgram :: MonadIO m => Program -> m [Kernel]
createKernelsInProgram prog = liftIO $
    getObjArray (clCreateKernelsInProgram prog)
        >>= mapM newKernel

{#fun clSetKernelArg as clSetKernelArg
  { withKernel* `Kernel'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  } -> `Int' checkSuccess*-
#}

-- API TODO
-- note doing withArg is silly since memobjs need manual release...
class KernelArg a where
    withKernelArg :: a -> (Int -> Ptr () -> IO b) -> IO b

withScalarArg :: Storable a => a -> (Int -> Ptr () -> IO b) -> IO b
withScalarArg x f = with x $ \p -> f (sizeOf x) (castPtr p)

instance KernelArg (Buffer a) where
    -- buffers are set the same as a scalar pointer would be.
    withKernelArg b f = withBuffer b $ \p -> withScalarArg p f

-- newtype eliminates need for UndecidableInstances
-- | A scalar argument, such as @float x@.
newtype Scalar a = Scalar a
instance Storable a => KernelArg (Scalar a) where
    withKernelArg (Scalar x) = withScalarArg x

-- TODO: Foreign.C types?
instance KernelArg Int8 where
    withKernelArg = withScalarArg

instance KernelArg Int16 where
    withKernelArg = withScalarArg

instance KernelArg Int32 where
    withKernelArg = withScalarArg

instance KernelArg Int64 where
    withKernelArg = withScalarArg

instance KernelArg Word8 where
    withKernelArg = withScalarArg

instance KernelArg Word16 where
    withKernelArg = withScalarArg

instance KernelArg Word32 where
    withKernelArg = withScalarArg

instance KernelArg Word64 where
    withKernelArg = withScalarArg

-- Float on modern platforms is IEEE, so this should be safe.
instance KernelArg Float where
    withKernelArg = withScalarArg

-- | A specification of a variable which is allocated in local memory and
-- shared by all work-items of a work-group.  For example: @__local float *x@.
newtype Local a = Local Int

instance Storable a => KernelArg (Local a) where
    withKernelArg (Local n) f = f (sizeOf (undefined :: a)*n) nullPtr

setKernelArg :: KernelArg a => Kernel -> Int -> a -> IO ()
setKernelArg kernel n x = withKernelArg x $ clSetKernelArg kernel n

data SomeArg = forall a . KernelArg a => SomeArg a

setKernelArgs :: Kernel -> [SomeArg]-> IO ()
setKernelArgs k = zipWithM_ setter [0..]
  where
    setter n (SomeArg x) = setKernelArg k n x

infixr 6 &:
(&:) :: KernelArg a => a -> [SomeArg] -> [SomeArg]
x &: xs = SomeArg x : xs


{#fun clEnqueueNDRangeKernel
  { withCommandQueue* `CommandQueue'
  , withKernel* `Kernel'
  , `Int'
  , id `Ptr CULong' -- currently, must be null.
  , id `Ptr CULong' -- global work size
  , id `Ptr CULong' -- local work size
  , withEvents* `[Event]'&
  , eventPtr `EventPtr'
  } -> `Int' checkSuccess*-
#}

ndRangeKernel :: NDRange d => Kernel -> d -> Maybe d -> Command
ndRangeKernel kernel global local
    = mkCommand $ runKernel' kernel global local

-- TODO: check # of args with getinfo
runKernelWithArgs :: NDRange d => Kernel -> d -> Maybe d
                        -> [SomeArg] -> Command
runKernelWithArgs k global local as = mkCommand $ \q es ep -> let
            loop _ [] = runKernel' k global local q es ep
            loop c (SomeArg x:xs) = withKernelArg x $ \n p -> do
                            clSetKernelArg k c n p
                            loop (c+1) xs
            in loop 0 as

runKernel' :: NDRange d => Kernel -> d -> Maybe d
            -> CommandQueue -> [Event] -> EventPtr -> IO ()
runKernel' kernel globalWorkSize localWorkSize queue es ep
       = withArrayLen gsizes $ \dim globalSizes ->
          withLocalSizeArray dim $ \localSizes ->
            clEnqueueNDRangeKernel queue kernel dim nullPtr
                    globalSizes localSizes es ep
  where
    gsizes = rangeDims globalWorkSize
    withLocalSizeArray dim = case localWorkSize of
        Nothing -> ($ nullPtr)
        Just sizesD -> let lsizes = rangeDims sizesD
                       in if length lsizes /= length gsizes
                            || or (zipWith (>) lsizes gsizes)
                        then error $ "Incompatible global/local work sizes: "
                                 ++ show (gsizes,lsizes)
                        else withArray lsizes



class NDRange d where
    rangeDims :: d -> [CULong]

instance NDRange () where
    rangeDims () = [1]

instance NDRange Int where
    rangeDims n = [fromIntegral n]

instance NDRange CSize where
    rangeDims n = [fromIntegral n]

instance Integral a => NDRange (a,a) where
    rangeDims (x,y) = [fromIntegral x, fromIntegral y]

instance Integral a => NDRange (a,a,a) where
    rangeDims (x,y,z) = [fromIntegral x, fromIntegral y, fromIntegral z]




{#fun clEnqueueTask
 { withCommandQueue* `CommandQueue'
 , withKernel* `Kernel'
  , withEvents* `[Event]'&
  , eventPtr `EventPtr'
 } -> `Int' checkSuccess*-
#}


task :: Kernel -> Command
task kernel = mkCommand $ \queue -> clEnqueueTask queue kernel
---------
-- Queries

#c
enum CLKernelInfo {
    CLKernelFunctionName = CL_KERNEL_FUNCTION_NAME,
    CLKernelNumArgs = CL_KERNEL_NUM_ARGS,
    CLKernelReferenceCount = CL_KERNEL_REFERENCE_COUNT,
    CLKernelContext = CL_KERNEL_CONTEXT,
    CLKernelProgram = CL_KERNEL_PROGRAM,
};

enum CLKernelWorkGroupInfo {
    CLKernelWorkGroupSize = CL_KERNEL_WORK_GROUP_SIZE,
    CLKernelCompileWorkGroupSize = CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
    CLKernelLocalMemSize = CL_KERNEL_LOCAL_MEM_SIZE,
};
#endc
{#enum CLKernelInfo {} #}
{#enum CLKernelWorkGroupInfo {} #}

{#fun clGetKernelInfo as getInfo
  { withKernel* `Kernel'
  , cEnum `CLKernelInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#};

-- The Khronos spec says you can pass in device_id=NULL when
-- there's only one device associated with the kernel,
-- but it doesn't work for me on OS X.  So, we require that parameter.
{#fun clGetKernelWorkGroupInfo as getWorkGroupInfo
  { withKernel* `Kernel'
  , deviceIDPtr `DeviceID'
  , cEnum `CLKernelWorkGroupInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#};

kernelFunctionName :: Kernel -> String
kernelFunctionName k = getPureProp (getInfo k CLKernelFunctionName)

kernelNumArgs :: Kernel -> Int
kernelNumArgs k = getPureProp (getInfo k CLKernelNumArgs)

kernelContext :: Kernel -> Context
kernelContext k@(Kernel fp)
    = unsafePerformIO $ withForeignPtr fp $ \_ ->
            getProp (getInfo k CLKernelContext)
                >>= retainedCLContext

kernelProgram :: Kernel -> Program
kernelProgram k@(Kernel fp)
    = unsafePerformIO $ withForeignPtr fp $ \_ ->
            getProp (getInfo k CLKernelProgram)
                >>= retainedProgram

kernelWorkGroupSize :: Kernel -> DeviceID -> Size
kernelWorkGroupSize k d = getPureProp $ getWorkGroupInfo k d CLKernelWorkGroupSize

kernelCompileWorkGroupSize :: Kernel -> DeviceID -> (Size,Size,Size)
kernelCompileWorkGroupSize k d = unsafePerformIO $ do
    [x,y,z] <- getArrayN 3 (getWorkGroupInfo k d CLKernelWorkGroupSize)
    return (x,y,z)

getKernelLocalMemSize :: Kernel -> DeviceID -> IO ULong
getKernelLocalMemSize k d = getProp $ getWorkGroupInfo k d CLKernelLocalMemSize

----------------------
-- KernelFunc

class KernelFunc f where
    -- liftWith :: ( (b -> IO ()) -> IO ()) -> (b -> f) -> f
    applyKFunc :: ([SomeArg] -> Command) -> [SomeArg] -> f

instance KernelFunc Command where
    applyKFunc run as = run (reverse as)

instance (KernelArg a, KernelFunc f) => KernelFunc (a -> f) where
    applyKFunc run as = \a -> applyKFunc run (SomeArg a:as)

runKernel :: (NDRange d, KernelFunc f) => Kernel -> d -> Maybe d -> f
runKernel  k global local = applyKFunc (runKernelWithArgs k global local) []
