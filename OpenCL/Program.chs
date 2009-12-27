module OpenCL.Program(CLProgram,
                    createProgramWithSource,
                    buildProgram,
                    CLProgramBuildInfo(..),
                    CLBuildStatus,
                    clGetProgramBuildInfo,
                    getBuildLog,
                ) where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error

import Control.Applicative
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)

{#fun clCreateProgramWithSource as clCreateProgramWithSource
  { withCLContext* `CLContext'
  , `Int'
  , id `Ptr CString'
  , id `Ptr CULong'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLProgram' newCLProgram*
#}

newCLProgram = newData CLProgram clReleaseProgram

-- TODO: ignoring the return value...
foreign import ccall "&" clReleaseProgram :: Releaser CLProgram_

-- TODO: make sure this is safe
-- - exceptions
createProgramWithSource :: CLContext -> [B.ByteString] -> IO CLProgram
createProgramWithSource context bs = withByteStrings bs $ \cs -> do
    let (cstrs, strLens) = unzip cs
    withArrayLen cstrs $ \count cstrsArr -> do
    withArray (map toEnum strLens) $ \lenArr -> do
    prog <- clCreateProgramWithSource context
                    count cstrsArr lenArr
    return prog

withByteStrings :: [B.ByteString] -> ([CStringLen] -> IO a) -> IO a
withByteStrings [] f = f []
withByteStrings (b:bs) f = withByteStrings bs $ \cs ->
        unsafeUseAsCStringLen b $ \c -> f (c:cs)


{#fun clBuildProgram as clBuildProgram
  { withCLProgram* `CLProgram'
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
  { withCLProgram* `CLProgram'
  , clDeviceIDPtr `DeviceID'
  , cEnum `CLProgramBuildInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

getBuildLog :: CLProgram -> DeviceID -> IO String
getBuildLog prog device = allocaBytes {#sizeof size_t#} $ \retValueSize -> do
    size <- clGetProgramBuildInfo prog device ProgramBuildLog 0 nullPtr 
    allocaBytes size $ \p -> do
    size' <- clGetProgramBuildInfo prog device ProgramBuildLog size p
    peekCString (castPtr p)
