module OpenCL.Buffer where

#include <OpenCL/OpenCL.h>
{#import OpenCL.Helpers.Types#}
import OpenCL.Helpers.C2HS
import OpenCL.Error

-- OK, idea is:
-- eventually have a typeclass for objects which can
-- be put into buffers.

#c
enum CLMemFlags {
    CLMemReadWrite = CL_MEM_READ_WRITE,
    CLMemWriteOnly = CL_MEM_WRITE_ONLY,
    CLMemReadOnly = CL_MEM_READ_ONLY,
    CLMemUseHostPtr = CL_MEM_USE_HOST_PTR,
    CLMemAllocHostPtr = CL_MEM_ALLOC_HOST_PTR,
    CLMemCopyHostPtr = CL_MEM_COPY_HOST_PTR
};
#endc
{#enum CLMemFlags {} deriving (Show,Eq)#}

-- TODO: should I be using Long or size_T explicitly to prevent overflow?
{#fun clCreateBuffer as clCreateBuffer
  { clContextPtr `CLContext'
  , combineBitMasks `[CLMemFlags]'
  , `Int'
  , id `Ptr ()'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLMem' mkCLMem
#}
