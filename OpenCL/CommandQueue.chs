module OpenCL.CommandQueue where

#include <OpenCL/OpenCL.h>
import OpenCL.Helpers.Types
import OpenCL.Helpers.C2HS
import OpenCL.Error

#c
enum CLCommandQueueProperties {
    CLQueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
    CLQueueProfilingEnable = CL_QUEUE_PROFILING_ENABLE
};
#endc
{#enum CLCommandQueueProperties {} deriving (Show,Eq)#}

{#fun clCreateCommandQueue as clCreateCommandQueue
  { withCLContext* `CLContext'
  , clDeviceIDPtr `CLDeviceID'
  , combineBitMasks `[CLCommandQueueProperties]'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLCommandQueue' newCLCommandQueue*
#}

newCLCommandQueue = newData CLCommandQueue clReleaseCommandQueue
foreign import ccall "&" clReleaseCommandQueue :: Releaser CLCommandQueue_

{#fun clFlush as clFlush
  { withCLCommandQueue* `CLCommandQueue'
  } -> `CLInt' checkSuccess-
#}

{#fun clFinish as clFinish
  { withCLCommandQueue* `CLCommandQueue'
  } -> `CLInt' checkSuccess-
#}

