# From https://github.com/ros2/rcl_interfaces/blob/1bd7d0b36c79e9e6db6980301f3732fff51ddc50/rcl_interfaces/msg/ParameterEventDescriptors.msg

# This message contains descriptors of a parameter event.
# It was an atomic update.
# A specific parameter name can only be in one of the three sets.

ParameterDescriptor[] new_parameters
ParameterDescriptor[] changed_parameters
ParameterDescriptor[] deleted_parameters