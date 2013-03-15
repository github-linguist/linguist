%define		_myadminpath	/var/www/myadmin
%define		pkgrelease	rc1
%define		microrelease	1

Name:		phpMyAdmin
Version:	3.1.1
Release:	%{pkgrelease}.%{microrelease}
License:	GPL
Group:		Applications/Databases/Interfaces
Source0:	http://prdownloads.sourceforge.net/phpmyadmin/%{name}-%{version}-%{pkgrelease}.tar.bz2
Source1:	phpMyAdmin-http.conf
URL:		http://sourceforge.net/projects/phpmyadmin/
Requires:	mysql
Requires:	php-mysql
Buildarch:	noarch
#BuildRoot:	%{_tmppath}/%{name}-root

Summary:	phpMyAdmin - web-based MySQL administration

%description
phpMyAdmin can manage a whole MySQL-server (needs a super-user) but
also a single database. To accomplish the latter you'll need a
properly set up MySQL-user which can read/write only the desired
database. It's up to you to look up the appropiate part in the MySQL
manual. Currently phpMyAdmin can:
  - create and drop databases
  - create, copy, drop and alter tables
  - delete, edit and add fields
  - execute any SQL-statement, even batch-queries
  - manage keys on fields
  - load text files into tables
  - create (*) and read dumps of tables
  - export (*) and import data to CSV values
  - administer multiple servers and single databases
  - check referencial integrity
  - create complex queries automatically connecting required tables
  - create PDF graphics of your database layout
  - communicate in more than 38 different languages


%prep
%setup -q -n %{name}-%{version}-%{pkgrelease}


%build


%install
[ "${RPM_BUILD_ROOT}" != "/" ] && [ -d "${RPM_BUILD_ROOT}" ] &&	\
		rm -rf "${RPM_BUILD_ROOT}"

#	Create directories.

install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/{css,js,lang,libraries,themes}
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/libraries/{auth,dbg,dbi,engines}
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/libraries/{export,tcpdf,import}
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/libraries/transformations
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/libraries/tcpdf/font
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/themes/{darkblue_orange,original}
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/themes/darkblue_orange/{css,img}
install -d "${RPM_BUILD_ROOT}%{_myadminpath}"/themes/original/{css,img}

#	Install files.

install libraries/config.default.php					\
		"${RPM_BUILD_ROOT}%{_myadminpath}"/config.inc.php
install *.{php,ico} "${RPM_BUILD_ROOT}%{_myadminpath}"/
install ChangeLog LICENSE README "${RPM_BUILD_ROOT}%{_myadminpath}"/
install Documentation.html docs.css "${RPM_BUILD_ROOT}%{_myadminpath}"/
install css/* "${RPM_BUILD_ROOT}%{_myadminpath}/css"/
install js/* "${RPM_BUILD_ROOT}%{_myadminpath}/js"/
install lang/*.php "${RPM_BUILD_ROOT}%{_myadminpath}/lang"/
install libraries/*.php "${RPM_BUILD_ROOT}%{_myadminpath}/libraries"/
install libraries/auth/*.php "${RPM_BUILD_ROOT}%{_myadminpath}/libraries/auth"/
install libraries/dbg/*.php "${RPM_BUILD_ROOT}%{_myadminpath}/libraries/dbg"/
install libraries/dbi/*.php "${RPM_BUILD_ROOT}%{_myadminpath}/libraries/dbi"/
install libraries/engines/*.php					\
		"${RPM_BUILD_ROOT}%{_myadminpath}/libraries/engines"/
install libraries/export/*.php					\
		"${RPM_BUILD_ROOT}%{_myadminpath}/libraries/export"/
install libraries/tcpdf/*.php "${RPM_BUILD_ROOT}%{_myadminpath}/libraries/tcpdf"/
install libraries/tcpdf/font/*.{php,z}				\
		"${RPM_BUILD_ROOT}%{_myadminpath}/libraries/tcpdf/font"/
install libraries/import/*.php					\
		"${RPM_BUILD_ROOT}%{_myadminpath}/libraries/import"/
install libraries/transformations/*.php				\
		"${RPM_BUILD_ROOT}%{_myadminpath}/libraries/transformations"/
install themes/darkblue_orange/*.{php,png}			\
		"${RPM_BUILD_ROOT}%{_myadminpath}/themes/darkblue_orange"/
install themes/darkblue_orange/css/*.php			\
		"${RPM_BUILD_ROOT}%{_myadminpath}/themes/darkblue_orange/css"/
install themes/darkblue_orange/img/*.{png,ico}			\
		"${RPM_BUILD_ROOT}%{_myadminpath}/themes/darkblue_orange/img"/
install themes/original/*.{php,png}				\
		"${RPM_BUILD_ROOT}%{_myadminpath}/themes/original"/
install themes/original/css/*.php				\
		"${RPM_BUILD_ROOT}%{_myadminpath}/themes/original/css"/
install themes/original/img/*.{png,ico}				\
		"${RPM_BUILD_ROOT}%{_myadminpath}/themes/original/img"/

#	Create documentation directories.

DOCROOT="${RPM_BUILD_ROOT}%{_docdir}/%{name}-%{version}"
install -d "${DOCROOT}"
install -d "${DOCROOT}"/{lang,scripts,transformations}

#	Install documentation files.

install RELEASE-DATE-* "${DOCROOT}"/
install CREDITS ChangeLog INSTALL LICENSE "${DOCROOT}"/
install README TODO "${DOCROOT}"/
install Documentation.* docs.css "${DOCROOT}"/
install translators.html "${DOCROOT}"/
install lang/*.sh "${DOCROOT}"/lang/
install scripts/* "${DOCROOT}"/scripts/
install libraries/tcpdf/README "${DOCROOT}"/README.tcpdf
install libraries/import/README "${DOCROOT}"/README.import
install libraries/transformations/README "${DOCROOT}"/transformations/
install libraries/transformations/TEMPLATE* "${DOCROOT}"/transformations/
install libraries/transformations/*.sh "${DOCROOT}"/transformations/

#	Install configuration file for Apache.

install -d "${RPM_BUILD_ROOT}%{_sysconfdir}/httpd/conf.d"
install "%{SOURCE1}"						\
		"${RPM_BUILD_ROOT}%{_sysconfdir}/httpd/conf.d/phpMyAdmin.conf"

#	Generate non-configuration file list.

(cd "${RPM_BUILD_ROOT}"; ls -d ."%{_myadminpath}"/*) |
	sed -e '/\/config\.inc\.php$/d' -e 's/^.//' > files.list



%clean
[ "${RPM_BUILD_ROOT}" != "/" ] && [ -d "${RPM_BUILD_ROOT}" ] &&	\
		rm -rf "${RPM_BUILD_ROOT}"


%files -f files.list
%defattr(644, root, root, 755)
%doc %{_docdir}/%{name}-%{version}
%dir %{_myadminpath}
%attr(640,root,apache) %config(noreplace) %verify(not size mtime md5) %{_myadminpath}/config.inc.php
%config(noreplace) %verify(not size mtime md5) %{_sysconfdir}/httpd/conf.d/*


%changelog
* Thu Feb 23 2006 Patrick Monnerat <pm@datasphere.ch>
- Version 2.8.0-rc1.1.

* Thu Dec 22 2005 Patrick Monnerat <patrick.monnerat@econophone.ch>
- Path "nullpw" to allow trying connection with null password after failure.
- Version 2.7.0-pl1.1.

* Mon Aug 22 2005 Patrick Monnerat <patrick.monnerat@econophone.ch>
- Version 2.6.3-pl1.

* Wed Jul 21 2004 Patrick Monnerat <patrick.monnerat@econophone.ch>
- Version 2.5.7-pl1.

* Fri Nov 22 2002 Patrick Monnerat <patrick.monnerat@econophone.ch>
- Version 2.3.0-rc1.
