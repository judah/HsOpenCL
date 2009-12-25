module OpenCL.CommandQueue where

#include <OpenCL/OpenCL.h>
{#import OpenCL.Helpers.Types#}
import OpenCL.Helpers.C2HS
import OpenCL.Error

#c
enum CLCommandQueueProperties {
    CLQueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
    CLQueueProfilingEnable = CL_QUEUE_PROFILING_ENABLE
};
#endc
{#enum CLCommandQueueProperties {}#}

{#fun clCreateCommandQueue as clCreateCommandQueue
  { clContextPtr `CLContext'
  , clDeviceIDPtr `CLDeviceID'
  , combineBitMasks `[CLCommandQueueProperties]'
  , alloca- `Ptr CInt' checkSuccessPtr*-
  } -> `CLCommandQueue' mkCLCommandQueue
#}

{#fun clFlush as clFlush
  { clCommandQueuePtr `CLCommandQueue'
  } -> `CLInt' checkSuccess-
#}
