# From https://github.com/ros2/rosidl/blob/409437194a5c0b6a9aff9b62015342746803b5aa/rosidl_adapter/test/data/msg/Test.msg

# msg level doc

# field level doc
bool bool_value
byte byte_value  # field level doc, style 2
# combined styles
char char_value # combined styles, part 2
float32 float32_value
float64 float64_value
int8 int8_value
uint8 uint8_value
int16 int16_value
uint16 uint16_value
int32 int32_value
uint32 uint32_value
int64 int64_value
uint64 uint64_value

@optional
float32 multiline_optional

@optional float32 inline_optional
@optional float32 inline_default_optional 32.0
@optional float32 INLINE_CONSTANT=32.0