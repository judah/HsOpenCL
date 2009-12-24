{-# LANGUAGE ScopedTypeVariables #-}
module OpenCL.Context where

import OpenCL.Error
#include <OpenCL/OpenCL.h>
{#import OpenCL.Helpers.Types#}

import OpenCL.Helpers.C2HS

clCreateContext :: CLDeviceID -> IO CLContext
clCreateContext dev = withCLDeviceID dev $ \pd -> alloca $ \errP -> do
    cxt :: Ptr () <- {# call clCreateContext as clCreateContext_c #}
                nullPtr 1 pd nullFunPtr nullPtr errP
    peek errP >>= checkSuccess
    return (CLContext (castPtr cxt))
