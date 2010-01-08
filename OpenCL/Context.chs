{-# LANGUAGE ScopedTypeVariables #-}
module OpenCL.Context(
                    Context,
                    createContext,
                    createContextFromType,
                    contextDevices,
                        ) where

import OpenCL.Error
#include <OpenCL/OpenCL.h>
import OpenCL.Internal.Types
import OpenCL.Internal.C2HS
import OpenCL.Platform.Foreign

import Control.Applicative


{#fun clCreateContext as clCreateContext
  { id `Ptr CLong'
  , withDeviceIDs* `[DeviceID]'&
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `Context' newContext*
#}

createContext :: [DeviceID] -> IO Context
createContext devices = clCreateContext nullPtr devices nullFunPtr nullPtr

-- TODO: the DeviceType is actually a bitfield
{#fun clCreateContextFromType as clCreateContextFromType
  { id `Ptr CLong'
  , deviceTypeEnum `DeviceType'
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `Context' newContext*
#}

createContextFromType :: DeviceType -> IO Context
createContextFromType dtype = clCreateContextFromType nullPtr
                                dtype nullFunPtr nullPtr

{#fun clGetContextInfo as clGetContextInfo
  { withContext* `Context'
  , cEnum `CLContextInfo' -- todo do enum
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

-- The only really interesting property is the devices, for now.
#c
enum CLContextInfo {
    CLContextReferenceCount = CL_CONTEXT_REFERENCE_COUNT,
    CLContextDevices = CL_CONTEXT_DEVICES,
    CLContextProperties = CL_CONTEXT_PROPERTIES
};
#endc
{#enum CLContextInfo {} #}

contextDevices :: Context -> [DeviceID]
contextDevices context = map DeviceID
        $ getPureProp (clGetContextInfo context CLContextDevices)
