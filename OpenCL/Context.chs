{-# LANGUAGE ScopedTypeVariables #-}
module OpenCL.Context(
                    CLContext,
                    createContext,
                    createContextFromType,
                    clContextDevices,
                        ) where

import OpenCL.Error
#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Platform.Foreign

import Control.Applicative


{#fun clCreateContext as clCreateContext
  { id `Ptr CLong'
  , `Int'
  , castPtr `Ptr (Ptr CLDeviceID_)' -- TODO: allow more than one, I guess...
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `CLContext' newCLContext*
#}

createContext :: [CLDeviceID] -> IO CLContext
createContext devices = withArrayLen (map _clDeviceIDPtr devices) $ \n ps ->
            clCreateContext nullPtr n ps nullFunPtr nullPtr

-- TODO: the DeviceType is actually a bitfield
{#fun clCreateContextFromType as clCreateContextFromType
  { id `Ptr CLong'
  , deviceTypeEnum `CLDeviceType'
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `CLContext' newCLContext*
#}

createContextFromType :: CLDeviceType -> IO CLContext
createContextFromType dtype = clCreateContextFromType nullPtr
                                dtype nullFunPtr nullPtr

{#fun clGetContextInfo as clGetContextInfo
  { withCLContext* `CLContext'
  , cEnum `CLContextInfo' -- todo do enum
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

-- The only really interesting property is the devices, for now.
#c
enum CLContextInfo {
    CLContextRefenceCount = CL_CONTEXT_REFERENCE_COUNT,
    CLContextDevices = CL_CONTEXT_DEVICES,
    CLContextProperties = CL_CONTEXT_PROPERTIES
};
#endc
{#enum CLContextInfo {} #}

clContextDevices :: CLContext -> [CLDeviceID]
clContextDevices context = unsafePerformIO $ do
    size <- clGetContextInfo context CLContextDevices 0 nullPtr 
    let numDevices = size `div` sizeOf (undefined :: Ptr CLDeviceID_)
    allocaArray numDevices $ \p -> do
    clGetContextInfo context CLContextDevices size (castPtr p)
    map CLDeviceID <$> peekArray numDevices p
