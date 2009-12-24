#include "platform_helpers.h"

cl_int clGetDeviceInfoPtr (cl_device_id* device,
                        cl_device_info param_name,
                        size_t param_value_size,
                        void *param_value,
                        size_t *param_value_size_ret) {
    return clGetDeviceInfo(*device, param_name,param_value_size,
                            param_value, param_value_size_ret);
}
