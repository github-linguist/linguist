%{!?__pear: %{expand: %%global __pear %{_bindir}/pear}}
%{!?pear_metadir: %global pear_metadir %{pear_phpdir}}

%global pear_channel pear.symfony.com
%global pear_name    Yaml
%global php_min_ver  5.3.3

Name:             php-symfony2-%{pear_name}
Version:          2.2.1
Release:          1%{?dist}
Summary:          Symfony2 %{pear_name} Component

Group:            Development/Libraries
License:          MIT
URL:              http://symfony.com/doc/current/components/yaml/index.html
Source0:          http://%{pear_channel}/get/%{pear_name}-%{version}.tgz

BuildArch:        noarch

BuildRequires:    php-pear(PEAR)
BuildRequires:    php-channel(%{pear_channel})
# For tests
BuildRequires:    php-common >= %{php_min_ver}
BuildRequires:    php-pear(pear.phpunit.de/PHPUnit)
# For tests: phpci
BuildRequires:    php-ctype
BuildRequires:    php-date
BuildRequires:    php-iconv
BuildRequires:    php-json
BuildRequires:    php-mbstring
BuildRequires:    php-pcre
BuildRequires:    php-spl

Requires:         php-common >= %{php_min_ver}
Requires:         php-pear(PEAR)
Requires:         php-channel(%{pear_channel})
Requires(post):   %{__pear}
Requires(postun): %{__pear}
# phpci
Requires:         php-ctype
Requires:         php-date
Requires:         php-iconv
Requires:         php-json
Requires:         php-mbstring
Requires:         php-pcre
Requires:         php-spl

Provides:         php-pear(%{pear_channel}/%{pear_name}) = %{version}

%description
The Symfony2 YAML Component parses YAML strings to convert them to PHP arrays.
It is also able to convert PHP arrays to YAML strings.

YAML, YAML Ain't Markup Language, is a human friendly data serialization
standard for all programming languages. YAML is a great format for your
configuration files. YAML files are as expressive as XML files and as readable
as INI files.

The Symfony2 YAML Component implements the YAML 1.2 version of the
specification.


%prep
%setup -q -c

# Create PHPUnit autoloader
( cat <<'PHPUNIT_AUTOLOADER'
<?php

# This file was created by RPM packaging and is not part of the original
# Symfony2 %{pear_name} PEAR package.

set_include_path(
    '%{pear_testdir}/%{pear_name}'.PATH_SEPARATOR.
    get_include_path()
);

spl_autoload_register(function ($class) {
    if ('\\' == $class[0]) {
        $class = substr($class, 1);
    }

    $file = str_replace('\\', '/', $class).'.php';
    @include $file;
});
PHPUNIT_AUTOLOADER
) > phpunit.autoloader.php

# Update PHPUnit config
sed -e 's#vendor/autoload.php#./phpunit.autoloader.php#' \
    -i %{pear_name}-%{version}/Symfony/Component/%{pear_name}/phpunit.xml.dist

# Modify PEAR package.xml file:
# - Remove .gitattributes file
# - Remove .gitignore file
# - Change role from "php" to "doc" for CHANGELOG.md file
# - Change role from "php" to "test" for all test files
# - Remove md5sum from phpunit.xml.dist file since it was updated
sed -e '/\.gitattributes/d' \
    -e '/\.gitignore/d' \
    -e '/CHANGELOG.md/s/role="php"/role="doc"/' \
    -e '/Tests/s/role="php"/role="test"/' \
    -e '/phpunit.xml.dist/s/role="php"/role="test"/' \
    -e '/phpunit.xml.dist/s/md5sum="[^"]*"\s*//' \
    -i package.xml

# package.xml is version 2.0
mv package.xml %{pear_name}-%{version}/%{name}.xml


%build
# Empty build section, nothing required


%install
cd %{pear_name}-%{version}
%{__pear} install --nodeps --packagingroot %{buildroot} %{name}.xml

# Clean up unnecessary files
rm -rf %{buildroot}%{pear_metadir}/.??*

# Install XML package description
mkdir -p %{buildroot}%{pear_xmldir}
install -pm 644 %{name}.xml %{buildroot}%{pear_xmldir}

# Install PHPUnit autoloader
install -pm 0644 ../phpunit.autoloader.php \
    %{buildroot}/%{pear_testdir}/%{pear_name}/Symfony/Component/%{pear_name}/


%check
cd %{pear_name}-%{version}/Symfony/Component/%{pear_name}

cp ../../../../phpunit.autoloader.php .

%{_bindir}/phpunit \
    -d include_path="%{buildroot}%{pear_phpdir}:%{buildroot}%{pear_testdir}/%{pear_name}:.:%{pear_phpdir}:%{_datadir}/php" \
    -d date.timezone="UTC" \
    || : Temporarily ignore failed tests


%post
%{__pear} install --nodeps --soft --force --register-only \
    %{pear_xmldir}/%{name}.xml >/dev/null || :


%postun
if [ $1 -eq 0 ] ; then
    %{__pear} uninstall --nodeps --ignore-errors --register-only \
        %{pear_channel}/%{pear_name} >/dev/null || :
fi


%files
%doc %{pear_docdir}/%{pear_name}
%{pear_xmldir}/%{name}.xml
%dir %{pear_phpdir}/Symfony
%dir %{pear_phpdir}/Symfony/Component
     %{pear_phpdir}/Symfony/Component/%{pear_name}
%{pear_testdir}/%{pear_name}


%changelog
* Sun Apr 14 2013 Shawn Iwinski <shawn.iwinski@gmail.com> 2.2.1-1
- Updated to 2.2.1

* Wed Mar 13 2013 Shawn Iwinski <shawn.iwinski@gmail.com> 2.2.0-1
- Updated to 2.2.0
- Removed tests' bootstrap patch

* Sun Mar 03 2013 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.8-1
- Updated to upstream version 2.1.8
- Temp disable tests unless "--with tests"

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.1.7-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Fri Jan 18 2013 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.7-1
- Updated to upstream version 2.1.7

* Sun Dec 23 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.6-1
- Updated to upstream version 2.1.6

* Sat Dec 22 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.5-1
- Updated to upstream version 2.1.5

* Fri Nov 30 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.4-1
- Updated to upstream version 2.1.4

* Tue Nov 13 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.3-2
- Removed .gitattributes and .gitignore files from package.xml

* Sun Nov 11 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.3-1
- Updated to upstream version 2.1.3

* Mon Oct 29 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.2-2
- Added "%%global pear_metadir" and usage in %%install
- Changed RPM_BUILD_ROOT to %%{buildroot}

* Sat Oct 20 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.1.2-1
- Updated to upstream version 2.1.2
- PHP minimum version 5.3.3 instead of 5.3.2
- Added PEAR package.xml modifications
- Added patch for tests' bootstrap.php
- Added tests (%%check)

* Sat Sep 15 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.17-1
- Updated to upstream version 2.0.17
- Added php-spl require

* Sat Jul 21 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 2.0.16-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Sun Jul 15 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.16-1
- Updated to upstream version 2.0.16
- Minor syntax updates

* Wed May 30 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.15-1
- Updated to upstream version 2.0.15
- Removed "BuildRequires: php-pear >= 1:1.4.9-1.2"
- Updated %%prep section
- Removed cleaning buildroot from %%install section
- Removed documentation move from %%install section (fixed upstream)
- Removed %%clean section
- Updated %%doc in %%files section

* Sun May 20 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.14-3
- Moved documentation to correct location

* Sun May 20 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.14-2
- Removed BuildRoot
- Changed php require to php-common
- Added the following requires based on phpci results:
  php-ctype, php-date, php-iconv, php-json, php-mbstring, php-pcre
- Removed %%defattr from %%files section

* Fri May 18 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.14-1
- Updated to upstream version 2.0.14
- %%global instead of %%define
- Removed unnecessary cd from %%build section

* Wed May 2 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.13-1
- Updated to upstream version 2.0.13

* Sat Apr 21 2012 Shawn Iwinski <shawn.iwinski@gmail.com> 2.0.12-1
- Initial package
