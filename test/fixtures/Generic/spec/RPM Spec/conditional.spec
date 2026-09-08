# Packaging settings precede the package headers.
%global package_name metrics-agent
%global package_version 2.4

Summary:        Collect system metrics
%if 0%{?fedora}
Release:        2%{?dist}
%else
Release:        1
%endif

License:        MIT
name:           %{package_name}
# Keep the version independent of distribution-specific release numbers.
version:        %{package_version}

%description
Collect system metrics for the monitoring service.

%package devel
Summary:        Development files for %{name}

%description devel
Headers for integrating with the monitoring service.

%files
%{_bindir}/metrics-agent
