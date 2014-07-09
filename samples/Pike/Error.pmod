#pike __REAL_VERSION__

constant Generic = __builtin.GenericError;

constant Index = __builtin.IndexError;

constant BadArgument = __builtin.BadArgumentError;

constant Math = __builtin.MathError;

constant Resource = __builtin.ResourceError;

constant Permission = __builtin.PermissionError;

constant Decode = __builtin.DecodeError;

constant Cpp = __builtin.CppError;

constant Compilation = __builtin.CompilationError;

constant MasterLoad = __builtin.MasterLoadError;

constant ModuleLoad = __builtin.ModuleLoadError;

//! Returns an Error object for any argument it receives. If the
//! argument already is an Error object or is empty, it does nothing.
object mkerror(mixed error)
{
  if (error == UNDEFINED)
    return error;
  if (objectp(error) && error->is_generic_error)
    return error;
  if (arrayp(error))
    return Error.Generic(@error);
  if (stringp(error))
    return Error.Generic(error);
  return Error.Generic(sprintf("%O", error));
}