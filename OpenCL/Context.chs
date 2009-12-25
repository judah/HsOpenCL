{-# LANGUAGE ScopedTypeVariables #-}
module OpenCL.Context where

import OpenCL.Error
#include <OpenCL/OpenCL.h>
{#import OpenCL.Helpers.Types#}
import OpenCL.Helpers.C2HS

import Control.Applicative

{-
{#fun clCreateContext as clCreateContext
  { id `Ptr ()'
  , `Int'
  , with- `CLDeviceID' -- TODO: allow more than one, I guess...
  , castFunPtr `FunPtr ()'
  , id `Ptr ()'
  , alloca- `CInt' checkSuccessPtr*-
  } -> `CLContext' id
#}
-}
clCreateContext :: CLDeviceID -> IO CLContext
clCreateContext dev = with (clDeviceIDPtr dev) $ \pd -> alloca $ \errP -> do
    cxt :: Ptr () <- {# call clCreateContext as clCreateContext_c #}
                nullPtr 1 pd nullFunPtr nullPtr errP
    peek errP >>= checkSuccess
    return (CLContext (castPtr cxt))

