// The SIP implementation of the docstrings_module test module.


%DefaultDocstringFormat "deindented"

%Module(name=docstrings_module, gil_use="NotUsed",
        multi_interpreter_support="PerInterpreterGILSupported")
{
%Docstring
Module
%End
};

%MinimumABIVersion "12.0"
%MinimumABIVersion "13.0"
%MinimumABIVersion "14.0"


%ModuleCode
void overloaded() {}
void overloaded(const char *arg) {}
void overloaded(int arg) {}
void overloaded(double arg) {}

void pod_ptr_arg(const char *arg) {}
void pod_ptr_opt_arg(const char *arg) {}
void bool_opt_arg(bool arg) {}
%End

%ModuleHeaderCode
void overloaded();
void overloaded(const char *arg);
void overloaded(int arg);
void overloaded(double arg);

void pod_ptr_arg(const char *arg);
void pod_ptr_opt_arg(const char *arg = NULL);
void bool_opt_arg(bool arg = true);

class Klass
{
public:
    Klass() {}
    Klass(const Klass &other) {}

    void callable() {}
};
%End


void overloaded();

void overloaded(const char *arg);
%Docstring(signature="prepended")
    prepended
%End

void overloaded(int arg /Constrained/);
%Docstring(signature="appended")
    appended
%End

void overloaded(double arg);
%Docstring(signature="discarded")
    discarded
%End

void pod_ptr_arg(const char *arg);
void pod_ptr_opt_arg(const char *arg = NULL);
void bool_opt_arg(bool arg = true);

class Klass
{
%Docstring(signature="appended")
    Klass overview
%End

public:
    Klass();
    %Docstring(signature="prepended")
        Default constructor
    %End

    Klass(const Klass &other);
    %Docstring(signature="prepended")
        Copy constructor
    %End

    void callable();
    %Docstring
        callable
    %End
};
