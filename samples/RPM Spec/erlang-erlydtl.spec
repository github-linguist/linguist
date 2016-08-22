%global debug_package %{nil}

Name:           erlang-erlydtl
Version:        0.6.0
Release:        1%{?dist}
Summary:        Erlang implementation of the Django Template Language.

Group:          Development/Libraries
License:        MIT
URL:            http://code.google.com/p/erlydtl/
Source0:        http://erlydtl.googlecode.com/files/erlydtl-0.6.0.tar.gz
Patch0:         erlang-erlydtl-0.6.0-tests.patch
Patch1:         erlang-erlydtl-0.6.0-r14a.patch
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

Provides:       ErlyDTL = %{version}-%{release}
BuildRequires:  erlang
Requires:       erlang

%description
ErlyDTL is an Erlang implementation of the Django Template Language. The
erlydtl module compiles Django Template source code into Erlang bytecode. The
compiled template has a "render" function that takes a list of variables and
returns a fully rendered document

%prep
%setup -q -n erlydtl-%{version}
find examples/ -type f -executable -exec chmod -x {} \;

%patch0 -p0
%patch1 -p0

%build
make %{?_smp_mflags}

%check
make test


%install
rm -rf %{buildroot}
mkdir -p %{buildroot}/%{_libdir}/erlang/lib/erlydtl-%{version}/
cp -r ebin     %{buildroot}/%{_libdir}/erlang/lib/erlydtl-%{version}/
cp -r bin      %{buildroot}/%{_libdir}/erlang/lib/erlydtl-%{version}/
cp -r priv     %{buildroot}/%{_libdir}/erlang/lib/erlydtl-%{version}/


%clean
rm -rf %{buildroot}


%files
%defattr(-,root,root,-)
%dir %{_libdir}/erlang/lib/erlydtl-%{version}  
%{_libdir}/erlang/lib/erlydtl-%{version}/*
%doc README
%doc examples


%changelog
* Sun Aug 1 2010 Ilia Cheishvili <ilia.cheishvili@gmail.com> - 0.6.0-1
- Initial Package
