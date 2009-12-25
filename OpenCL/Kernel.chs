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

setKernelMemArg :: CLKernel -> Int -> Int -> CLMem -> IO ()
setKernelMemArg kernel arg size mem
    = clSetKernelArg kernel arg size (clMemPtr mem)
