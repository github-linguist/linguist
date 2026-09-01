# From OpenSCAP/openscap openscap.spec
Name:           openscap
Release:        0%{?dist}
Version:        1.3.0
Epoch:          1
Summary:        SCAP integration libraries
License:        LGPLv2+
%if %{?_with_check:1}%{!?_with_check:0}
BuildRequires:  python3-dbusmock
%endif

%description
OpenSCAP provides an integration path for SCAP standards.
