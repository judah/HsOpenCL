module OpenCL.Kernel where

#include <OpenCL/OpenCL.h>
{#import OpenCL.Helpers.Types#}
import OpenCL.Helpers.C2HS
import OpenCL.Error

{#fun clCreateKernel as clCreateKernel
  { clProgramPtr `CLProgram'
  , `String'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLKernel' mkCLKernel
#}

{#fun clSetKernelArg as clSetKernelArg
  { clKernelPtr `CLKernel'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  } -> `Int' checkSuccess-
#}

setKernelMemArg :: CLKernel -> Int -> CLMem -> IO ()
setKernelMemArg kernel arg mem
    = with (clMemPtr mem) $ 
        clSetKernelArg kernel arg {#sizeof cl_mem#}

{#fun clEnqueueNDRangeKernel as clEnqueueNDRangeKernel
  { clCommandQueuePtr `CLCommandQueue'
  , clKernelPtr `CLKernel'
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
