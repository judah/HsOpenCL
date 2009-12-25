module OpenCL.Kernel where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
{#import OpenCL.Program#}
{#import OpenCL.CommandQueue#}
{#import OpenCL.Buffer#}
import OpenCL.Helpers.C2HS
import OpenCL.Error


{#pointer cl_kernel as CLKernel newtype#}

{#fun clCreateKernel as clCreateKernel
  { id `CLProgram'
  , `String'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLKernel' id
#}

{#fun clSetKernelArg as clSetKernelArg
  { id `CLKernel'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  } -> `Int' checkSuccess-
#}

setKernelMemArg :: CLKernel -> Int -> CLMem -> IO ()
setKernelMemArg kernel arg (CLMem mem)
    = with mem $ 
        clSetKernelArg kernel arg {#sizeof cl_mem#}

{#fun clEnqueueNDRangeKernel as clEnqueueNDRangeKernel
  { id `CLCommandQueue'
  , id `CLKernel'
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
