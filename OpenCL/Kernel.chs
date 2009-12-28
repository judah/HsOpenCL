module OpenCL.Kernel(
                Kernel,
                createKernel,
                setKernelMemArg,
                enqueueNDRangeKernel,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error


{#fun clCreateKernel as createKernel
  { withProgram* `Program'
  , `String'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Kernel' newKernel*
#}

newKernel = newData Kernel clReleaseKernel
foreign import ccall "&" clReleaseKernel :: Releaser Kernel_

{#fun clSetKernelArg as clSetKernelArg
  { withKernel* `Kernel'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  } -> `Int' checkSuccess-
#}

setKernelMemArg :: Kernel -> Int -> Buffer a -> IO ()
setKernelMemArg kernel arg mem = withBuffer mem $ \p -> with p $
        clSetKernelArg kernel arg {#sizeof cl_mem#}

{#fun clEnqueueNDRangeKernel as clEnqueueNDRangeKernel
  { withCommandQueue* `CommandQueue'
  , withKernel* `Kernel'
  , `Int'
  , id `Ptr CULong' -- currently, must be null.
  , id `Ptr CULong' -- global work size
  , id `Ptr CULong' -- local work size
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `Int' checkSuccess-
#}

enqueueNDRangeKernel :: CommandQueue -> Kernel -> [Int] -> IO ()
enqueueNDRangeKernel queue kernel globalWorkSize
    = withArrayLen (map toEnum globalWorkSize) $ \dim workSizes ->
        clEnqueueNDRangeKernel queue kernel dim nullPtr workSizes nullPtr 0 nullPtr
                nullPtr
