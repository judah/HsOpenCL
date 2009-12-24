#include "opencl_wrappers.h"

cl_int clGetDeviceInfoPtr (cl_device_id* device,
                        cl_device_info param_name,
                        size_t param_value_size,
                        void *param_value,
                        size_t *param_value_size_ret) {
    return clGetDeviceInfo(*device, param_name,param_value_size,
                            param_value, param_value_size_ret);
}
/*
void clCreateContextPtr (
            cl_context* cl_context_tbd,
            const cl_context_properties *properties,
            cl_uint num_devices, const cl_device_id *devices,
            void (*pfn_notify)(const char *errinfo,
                    const void *private_info, size_t cb, void *user_data),
            void *user_data, cl_int *errcode_ret) {
    *cl_context_tbd = clCreateContext(properties, num_devices, devices,
                                    pfn_notify, user_data, errcode_ret);
    return ();
}

*/       
