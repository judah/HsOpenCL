module OpenCL.Kernel(
                Kernel,
                createKernel,
                createKernelsInProgram,
                -- * Tasks
                -- ** Arguments
                KernelArg(),
                setKernelArg,
                Scalar(..),
                -- ** Running kernels
                enqueueNDRangeKernel,
                enqueueTask,
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
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error
import OpenCL.Platform(Size,ULong)

{#fun clCreateKernel as createKernel
  { withProgram* `Program'
  , `String'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Kernel' newKernel*
#}

newKernel = newData Kernel clReleaseKernel
foreign import ccall "&" clReleaseKernel :: Releaser Kernel_

{#fun clCreateKernelsInProgram
  { withProgram* `Program'
  , `Int'
  , id `Ptr (Ptr ())'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}


createKernelsInProgram :: Program -> IO [Kernel]
createKernelsInProgram prog = do
    n <- clCreateKernelsInProgram prog 0 nullPtr
    allocaArray n $ \p -> do
    clCreateKernelsInProgram prog n p
    peekArray n p >>= mapM newKernel

{#fun clSetKernelArg as clSetKernelArg
  { withKernel* `Kernel'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  } -> `Int' checkSuccess-
#}

-- API TODO
class KernelArg a where
    withKernelArg :: a -> (Int -> Ptr () -> IO ()) -> IO ()

instance KernelArg (Buffer a) where
    withKernelArg b f = withBuffer b $ \p -> withKernelArg (Scalar p) f

-- newtype eliminates need for UndecidableInstances
newtype Scalar a = Scalar a
instance Storable a => KernelArg (Scalar a) where
    withKernelArg (Scalar a) f = with a $ \p -> f (sizeOf a) (castPtr p)

setKernelArg :: KernelArg a => Kernel -> Int -> a -> IO ()
setKernelArg kernel n x = withKernelArg x $ clSetKernelArg kernel n

{#fun clEnqueueNDRangeKernel as clEnqueueNDRangeKernel
  { withCommandQueue* `CommandQueue'
  , withKernel* `Kernel'
  , `Int'
  , id `Ptr CULong' -- currently, must be null.
  , id `Ptr CULong' -- global work size
  , id `Ptr CULong' -- local work size
  , withEvents* `[Event]'&
  , alloca- `Event' newEvent*
  } -> `Int' checkSuccess-
#}

enqueueNDRangeKernel :: CommandQueue -> Kernel -> [Int] -> Maybe [Int]
                        -> [Event] -> IO Event
enqueueNDRangeKernel queue kernel globalWorkSize localWorkSize events
    = withArrayLen (map toEnum globalWorkSize) $ \dim globalSizes ->
        withLocalSizeArray dim $ \localSizes ->
            clEnqueueNDRangeKernel queue kernel dim nullPtr
                    globalSizes localSizes events
  where
    withLocalSizeArray dim = case localWorkSize of
        Nothing -> ($ nullPtr)
        Just sizes
            | length sizes /= dim
                -> error $ "enqueueNDRangeKernel: global and local sizes "
                            ++ "are not of the same dimensions"
            | otherwise -> withArray (map toEnum sizes)

{#fun clEnqueueTask as enqueueTask
 { withCommandQueue* `CommandQueue'
 , withKernel* `Kernel'
 , withEvents* `[Event]'&
 , alloca- `Event' newEvent*
 } -> `Int' checkSuccess-
#}

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
