{-# LANGUAGE ScopedTypeVariables #-}
module OpenCL.Context where

import OpenCL.Error
#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS

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

createContext :: CLDeviceID -> IO CLContext
createContext dev = with (castPtr $ clDeviceIDPtr dev) $ \pd ->
            clCreateContext nullPtr 1 pd nullFunPtr nullPtr

newCLContext = newData CLContext clReleaseContext
foreign import ccall "&" clReleaseContext :: Releaser CLContext_
