module OpenCL.Kernel where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
{#import OpenCL.CommandQueue#}
{#import OpenCL.Buffer#}
import OpenCL.Helpers.C2HS
import OpenCL.Error


{#fun clCreateKernel as clCreateKernel
  { withCLProgram* `CLProgram'
  , `String'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLKernel' newCLKernel*
#}

newCLKernel = newData CLKernel clReleaseKernel
foreign import ccall "&" clReleaseKernel :: Releaser CLKernel_

{#fun clSetKernelArg as clSetKernelArg
  { withCLKernel* `CLKernel'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  } -> `Int' checkSuccess-
#}

setKernelMemArg :: CLKernel -> Int -> CLMem -> IO ()
setKernelMemArg kernel arg mem = withCLMem mem $ \p -> with p $
        clSetKernelArg kernel arg {#sizeof cl_mem#}

{#fun clEnqueueNDRangeKernel as clEnqueueNDRangeKernel
  { withCLCommandQueue* `CLCommandQueue'
  , withCLKernel* `CLKernel'
  , `Int'
  , id `Ptr CULong' -- currently, must be null.
  , id `Ptr CULong' -- global work size
  , id `Ptr CULong' -- local work size
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `Int' checkSuccess-
#}

enqueueNDRangeKernel :: CLCommandQueue -> CLKernel -> [Int] -> IO ()
enqueueNDRangeKernel queue kernel globalWorkSize
    = withArrayLen (map toEnum globalWorkSize) $ \dim workSizes ->
        clEnqueueNDRangeKernel queue kernel dim nullPtr workSizes nullPtr 0 nullPtr
                nullPtr
