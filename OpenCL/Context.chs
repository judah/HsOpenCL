{-# LANGUAGE ScopedTypeVariables #-}
module OpenCL.Context where

import OpenCL.Error
#include <OpenCL/OpenCL.h>
{#import OpenCL.Helpers.Types#}
import OpenCL.Helpers.C2HS

import Control.Applicative

clCreateContext :: CLDeviceID -> IO CLContext
clCreateContext dev = with (clDeviceIDPtr dev) $ \pd -> alloca $ \errP -> do
    cxt :: Ptr () <- {# call clCreateContext as clCreateContext_c #}
                nullPtr 1 pd nullFunPtr nullPtr errP
    peek errP >>= checkSuccess
    return (CLContext (castPtr cxt))

-----------
-- Other stuff really ought to go in a separate module...

{#fun clCreateCommandQueue as clCreateCommandQueue
  { clContextPtr `CLContext'
  , clDeviceIDPtr `CLDeviceID'
  , id `CULLong' -- TODO: properties
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLCommandQueue' mkCLCommandQueue
#}



{#fun clCreateProgramWithSource as clCreateProgramWithSource
  { clContextPtr `CLContext'
  , `Int'
  , id `Ptr CString'
  , id `Ptr CULong'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLProgram' mkCLProgram
#}

-- TODO: make sure this is safe
-- TODO: Use ByteString
-- - exceptions
-- - freeing immediately is OK?
createProgramWithSource :: CLContext -> [String] -> IO CLProgram
createProgramWithSource context cs = do
    (cstrs, strLens) <- unzip <$> mapM newCStringLen cs
    withArrayLen cstrs $ \count cstrsArr -> do
    withArray (map toEnum strLens) $ \lenArr -> do
    prog <- clCreateProgramWithSource context
                    count cstrsArr lenArr
    mapM_ free cstrs
    return prog

{#fun clBuildProgram as clBuildProgram
  { clProgramPtr `CLProgram'
  , cEnum `Int'
  , castPtr `Ptr (Ptr _CLDeviceID)'
  , `String'
  , castFunPtr `FunPtr ()' -- notification
  , id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

buildProgram :: CLProgram -> IO ()
buildProgram prog = clBuildProgram prog 0 nullPtr "" nullFunPtr nullPtr

#c
enum CLProgramBuildInfo {
    ProgramBuildStatus = CL_PROGRAM_BUILD_STATUS,
    ProgramBuildOptions = CL_PROGRAM_BUILD_OPTIONS,
    ProgramBuildLog = CL_PROGRAM_BUILD_LOG
};
#endc
{#enum CLProgramBuildInfo {}#}

#c
enum CLBuildStatus {
    CLBuildNone = CL_BUILD_NONE,
    CLBuildError = CL_BUILD_ERROR,
    CLBuildSuccess = CL_BUILD_SUCCESS,
    CLBuildInProgramss = CL_BUILD_IN_PROGRESS
};
#endc
{#enum CLBuildStatus {} deriving (Show,Eq)#}
    

{#fun clGetProgramBuildInfo as clGetProgramBuildInfo
  { clProgramPtr `CLProgram'
  , clDeviceIDPtr `CLDeviceID'
  , cEnum `CLProgramBuildInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

getBuildLog :: CLProgram -> CLDeviceID -> IO String
getBuildLog prog device = allocaBytes {#sizeof size_t#} $ \retValueSize -> do
    size <- clGetProgramBuildInfo prog device ProgramBuildLog 0 nullPtr 
    allocaBytes size $ \p -> do
    size' <- clGetProgramBuildInfo prog device ProgramBuildLog size p
    peekCString (castPtr p)
