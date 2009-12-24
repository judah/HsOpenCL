module OpenCL.Error(CLError,
               checkSuccess)  where

#include <OpenCL/OpenCL.h>

import Data.Typeable
import Control.Exception

import Foreign.C

#c
enum CLError {
    CLSuccess = CL_SUCCESS,
    CLDeviceNotFound = CL_DEVICE_NOT_FOUND,
    CLDeviceNotAvailable = CL_DEVICE_NOT_AVAILABLE,
    CLCompilerNotAvailable = CL_COMPILER_NOT_AVAILABLE,
    CLMemObjectAllocationFailure = CL_MEM_OBJECT_ALLOCATION_FAILURE,
    CLOutOfResources = CL_OUT_OF_RESOURCES,
    CLOutOfHostMemory = CL_OUT_OF_HOST_MEMORY,
    CLProfilingInfoNotAvailable = CL_PROFILING_INFO_NOT_AVAILABLE,
    CLMemCopyOverlap = CL_MEM_COPY_OVERLAP,
    CLImageFormatMismatch = CL_IMAGE_FORMAT_MISMATCH,
    CLImageFormatNotSupported = CL_IMAGE_FORMAT_NOT_SUPPORTED,
    CLBuildProgramFailure = CL_BUILD_PROGRAM_FAILURE,
    CLMapFailure = CL_MAP_FAILURE,
    CLInvalidValue = CL_INVALID_VALUE,
    CLInvalidDeviceType = CL_INVALID_DEVICE_TYPE,
    CLInvalidPlatform = CL_INVALID_PLATFORM,
    CLInvalidDevice = CL_INVALID_DEVICE,
    CLInvalidContext = CL_INVALID_CONTEXT,
    CLInvalidQueueProperties = CL_INVALID_QUEUE_PROPERTIES,
    CLInvalidCommandQueue = CL_INVALID_COMMAND_QUEUE,
    CLInvalidHostPtr = CL_INVALID_HOST_PTR,
    CLInvalidMemObject = CL_INVALID_MEM_OBJECT,
    CLInvalidImageFormatDescriptor = CL_INVALID_IMAGE_FORMAT_DESCRIPTOR,
    CLInvalidImageSize = CL_INVALID_IMAGE_SIZE,
    CLInvalidSampler = CL_INVALID_SAMPLER,
    CLInvalidBinary = CL_INVALID_BINARY,
    CLInvalidBuildOptions = CL_INVALID_BUILD_OPTIONS,
    CLInvalidProgram = CL_INVALID_PROGRAM,
    CLInvalidProgramExecutable = CL_INVALID_PROGRAM_EXECUTABLE,
    CLInvalidKernelName = CL_INVALID_KERNEL_NAME,
    CLInvalidKernelDefinition = CL_INVALID_KERNEL_DEFINITION,
    CLInvalidKernel = CL_INVALID_KERNEL,
    CLInvalidArgIndex = CL_INVALID_ARG_INDEX,
    CLInvalidArgValue = CL_INVALID_ARG_VALUE,
    CLInvalidArgSize = CL_INVALID_ARG_SIZE,
    CLInvalidKernelArgs = CL_INVALID_KERNEL_ARGS,
    CLInvalidWorkDimension = CL_INVALID_WORK_DIMENSION,
    CLInvalidWorkGroupSize = CL_INVALID_WORK_GROUP_SIZE,
    CLInvalidWorkItemSize = CL_INVALID_WORK_ITEM_SIZE,
    CLInvalidGlobalOffset = CL_INVALID_GLOBAL_OFFSET,
    CLInvalidEventWaitList = CL_INVALID_EVENT_WAIT_LIST,
    CLInvalidEvent = CL_INVALID_EVENT,
    CLInvalidOperation = CL_INVALID_OPERATION,
    CLInvalidGlObject = CL_INVALID_GL_OBJECT,
    CLInvalidBufferSize = CL_INVALID_BUFFER_SIZE,
    CLInvalidMipLevel = CL_INVALID_MIP_LEVEL,

};
#endc
{#enum CLError {} deriving (Show,Typeable)#}
instance Exception CLError where

-- TODO: CLSuccess shouldn't really be an error...
-- TODO: better error message if unknown error
-- TODO: add String argument like Foreign.C.Error funcs
--       so we have better messages.
checkSuccess :: CInt -> IO ()
checkSuccess n
    | n' == fromEnum CLSuccess = return ()
    | otherwise = throw (toEnum n' :: CLError)
  where n' = fromEnum n
