#
# spec file for package manos
#
# Copyright (c) 2010 Jackson Harper (jackson@novell.com)
#
#

Name:           manos-devel
Version:        0.1.1
Release:        1
License:        MIT/X11
BuildRoot:      %{_tmppath}/manos-%{version}-build
BuildRequires:  mono-devel >= 2.6
BuildRequires:  mono-nunit >= 2.6
Source0:        manos-%{version}.tar.bz2
Source1:        rpmlintrc
Summary:        The Manos Web Application Framework
Group:          Development/Web/Servers
BuildArch:      noarch

%description
Manos is an easy to use, easy to test, high performance web application framework that stays out of your way and makes your life ridiculously simple.

%files
%defattr(-, root, root)
%{_prefix}/lib/manos
%{_bindir}/manos
%{_datadir}/manos
%{_prefix}/lib/pkgconfig/manos.pc
%{_datadir}/man/man1/manos.1.gz

%prep
%setup -q -n manos-%{version}


%build
./configure --prefix=%{buildroot}%{_prefix} --install-prefix=%{_prefix}
make

%install
make install

%clean
rm -rf %{buildroot}

%changelog