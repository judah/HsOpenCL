#include <OpenCL/OpenCL.h>

cl_int clGetDeviceInfoPtr (cl_device_id* device,
                        cl_device_info param_name,
                        size_t param_value_size,
                        void *param_value,
                        size_t *param_value_size_ret);
