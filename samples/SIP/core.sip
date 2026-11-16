// Define the SIP wrapper to the core library.

%Module(name=examples.core, use_limited_api=True, gil_use="NotUsed",
        multi_interpreter_support="Supported")

%MinimumABIVersion "12.0"
%MinimumABIVersion "13.0"
%MinimumABIVersion "14.0"

%DefaultEncoding "ASCII"

%Platforms {Linux macOS Windows}

%If (Linux)
const char *what_am_i();
%MethodCode
    sipRes = "Linux";
%End
%End

%If (macOS)
const char *what_am_i();
%MethodCode
    sipRes = "macOS";
%End
%End

%If (Windows)
const char *what_am_i();
%MethodCode
    sipRes = "Windows";
%End
%End
