// mds_utils/python/dictionary.i
// 
// Copyright (c) 2014 - Michele De Stefano (micdestefano@users.sourceforge.net)
// 
// Distributed under the MIT License (See accompanying file LICENSE)

%include "mds_utils/python/obj.i"

%typemap(in) (mds_utils::python::Dictionary) {
	try {
		if (!PyDict_Check($input)) {
			throw std::invalid_argument("The dictionary argument is not a dictionary.");
		}
		$1 = $input;
	} catch (std::exception& e) {
		PyErr_SetString(PyExc_RuntimeError,e.what());
        SWIG_fail;
	}
}

%typemap(out) mds_utils::python::Dictionary = mds_utils::python::Obj;

%typecheck(SWIG_TYPECHECK_POINTER) mds_utils::python::Dictionary {
  $1 = PyDict_Check($input);
}
