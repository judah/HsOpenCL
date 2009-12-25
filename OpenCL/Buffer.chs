module OpenCL.Buffer where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
{#import OpenCL.CommandQueue#}
import OpenCL.Helpers.C2HS
import OpenCL.Error

{#pointer cl_mem as CLMem newtype#}

-- OK, idea is:
-- eventually have a typeclass for objects which can
-- be put into & taken out of buffers.
-- (IE lists, Arrays, etc.)
-- (maybe purity when read-only?)

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
  } -> `CLMem' id
#}

createBuffer :: forall a . Storable a => CLContext -> [CLMemFlags] -> Int -> Ptr a -> IO CLMem
createBuffer context flags size p
    = clCreateBuffer context flags (size * sizeOf (undefined :: a)) nullPtr

{#fun clEnqueueReadBuffer as clEnqueueReadBuffer
  { id `CLCommandQueue'
  , id `CLMem'
  , cFromBool `Bool'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `CLInt' checkSuccess-
#}

enqueueReadBuffer :: forall a . Storable a => CLCommandQueue -> CLMem -> Int -> Ptr a
                        -> IO ()
enqueueReadBuffer queue mem size p
    = clEnqueueReadBuffer queue mem True 0 (size * sizeOf (undefined :: a)) p
                            0 nullPtr nullPtr

{#fun clEnqueueWriteBuffer as clEnqueueWriteBuffer
  { id `CLCommandQueue'
  , id `CLMem'
  , cFromBool `Bool'
  , `Int'
  , `Int'
  , castPtr `Ptr a'
  , `Int'
  , id `Ptr (Ptr ())'
  , id `Ptr (Ptr ())'
  } -> `CLInt' checkSuccess-
#}

enqueueWriteBuffer :: forall a . Storable a => CLCommandQueue -> CLMem -> Int -> Ptr a
                        -> IO ()
enqueueWriteBuffer queue mem size p
    = clEnqueueWriteBuffer queue mem True 0 (size * sizeOf (undefined :: a)) p
                            0 nullPtr nullPtr

