module OpenCL.Program(Program,
                    createProgramWithSource,
                    buildProgram,
                    BuildStatus(..),
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
  { withContext* `Context'
  , `Int'
  , id `Ptr CString'
  , id `Ptr CULong'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `Program' newCLProgram*
#}

newCLProgram = newData Program clReleaseProgram

-- TODO: ignoring the return value...
foreign import ccall "&" clReleaseProgram :: Releaser Program_

-- TODO: make sure this is safe
-- - exceptions
createProgramWithSource :: Context -> [B.ByteString] -> IO Program
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
  { withProgram* `Program'
  , cEnum `Int'
  , castPtr `Ptr (Ptr _DeviceID)'
  , `String'
  , castFunPtr `FunPtr ()' -- notification
  , id `Ptr ()'
  } -> `Int' checkSuccess*-
#}

buildProgram :: Program -> IO ()
buildProgram prog = clBuildProgram prog 0 nullPtr "" nullFunPtr nullPtr

#c
enum CLProgramBuildInfo {
    CLProgramBuildStatus = CL_PROGRAM_BUILD_STATUS,
    CLProgramBuildOptions = CL_PROGRAM_BUILD_OPTIONS,
    CLProgramBuildLog = CL_PROGRAM_BUILD_LOG
};
#endc
{#enum CLProgramBuildInfo {}#}

#c
enum BuildStatus {
    BuildNone = CL_BUILD_NONE,
    BuildError = CL_BUILD_ERROR,
    BuildSuccess = CL_BUILD_SUCCESS,
    BuildInProgramss = CL_BUILD_IN_PROGRESS
};
#endc
{#enum BuildStatus {} deriving (Show,Eq)#}
    

{#fun clGetProgramBuildInfo as clGetProgramBuildInfo
  { withProgram* `Program'
  , deviceIDPtr `DeviceID'
  , cEnum `CLProgramBuildInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

-- TODO: should be ByteString?
getBuildLog :: Program -> DeviceID -> IO String
getBuildLog prog device = getProp $
        clGetProgramBuildInfo prog device CLProgramBuildLog
