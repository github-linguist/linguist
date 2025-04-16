%define _prefix /usr/local
%define _mandir /usr/local/man
%define _sysconfdir /etc

%define apache_ver      1.3.42
%define mod_ssl_ver     2.8.31
%define	mod_perl_ver    1.31
%define	libapreq_ver    1.34


%define aname           apache
%define pname           httpd13
%define contentdir      %{_var}/www
%define suexec_caller   apache

Summary:       The 1.x Apache webserver (with static mod_perl, mod_ssl)
Name:          apache
Version:       %{apache_ver}
Release:       4%{?dist}
License:       Apache Software License 2.0
URL:           http://httpd.apache.org/
Group:         System Environment/Daemons

Requires:      initscripts >= 3.25
Requires:      openssl >= 0.9.6

BuildRequires: openssl-devel mm-devel krb5-devel pkgconfig 
BuildRequires: perl-ExtUtils-MakeMaker perl-libwww-perl perl-HTML-Parser perl-ExtUtils-Embed
BuildRequires: gdbm-devel flex 
Requires:      /sbin/chkconfig /bin/mktemp /usr/sbin/useradd
Requires:      findutils procps

Provides:      webserver
Provides:      mod_perl = %{mod_perl_ver}
Provides:      perl(mod_perl) = %{mod_perl_ver}
Provides:      mod_ssl = %{mod_ssl_ver}
Provides:      apache = %{apache_ver}

Source0:       http://httpd.apache.org/dist/apache_%{apache_ver}.tar.bz2
Source1:       http://www.modssl.org/source/mod_ssl-%{mod_ssl_ver}-%{apache_ver}.tar.gz
Source2:       http://perl.apache.org/dist/mod_perl-%{mod_perl_ver}.tar.gz
Source3:       httpd.init
Source4:       apache.logrotate
Source5:       SSL-Certificate-Creation
Source6:       ftp://ftp.cpan.org/authors/id/J/JO/JOESUF/libapreq-%{libapreq_ver}.tar.gz

Patch0:        sslcfg.patch
Patch1:        apache_1.3.39-config.patch
Patch3:        apache_1.3.39-Makefile.patch
Patch5:        apache_1.3.20-apachectl-init.patch
Patch11:       mod_ssl-2.8.4-openssl.patch
Patch12:       apache_1.3.42-db.patch
Patch13:       apache-1.3.39-gcc44.patch
Patch14:       mod_ssl-2.8.31-STACK.patch
Patch15:       apache_1.3.39-ap_getline.patch
Patch16:       mod_ssl-openssl-x86_64.patch
Patch17:       mp1+perl5.14.diff
Patch18:       apache_1.3.42-64bits.patch


%description
This package contains a powerful, full-featured, efficient, and
freely-available Web server based on work done by the Apache Software
Foundation. It is also the most popular Web server on the Internet.

------------------------------------------------------------------------------
This package is a custom release containing the httpd server (v%{apache_ver})
bundled with: mod_perl v.%{mod_ssl_ver},
and mod_ssl v%{mod_ssl_ver}, all BUILT-IN.
------------------------------------------------------------------------------

	The Apache/Perl integration project brings together the full power
of the Perl programming language and the Apache HTTP server.
	With mod_perl it is possible to write Apache modules entirely in Perl.
In addition, the persistent interpreter embedded in the server avoids the
overhead of starting an external interpreter and the penalty of Perl
start-up time.
	Mod_SSL provides strong cryptography for the Apache 1.3 webserver
via the Secure Sockets Layer (SSL v2/v3) and Transport Layer Security
(TLSv1) protocols by the help of the Open Source SSL/TLS toolkit OpenSSL.


%package devel
Group:         Development/Libraries
Summary:       Module development tools for apache-mod_ssl
Provides:      eapi = %{mod_ssl_ver}
Requires:      %{name} = %{version}
Provides:      mod_perl-devel = %{mod_perl_ver}
Provides:      mod_ssl-devel = %{mod_ssl_ver}
Provides:      apache-devel = %{apache_ver}

%description devel
The apache-devel package contains the APXS binary and other files
that you need to build Dynamic Shared Objects (DSOs) for Apache.
If you are installing the Apache Web server and you want to be able
to compile or develop additional modules for Apache, you need to install
this package.


%package manual
Group:         Documentation
Summary:       Documentation for the Apache Web server

%description manual
The apache-manual package contains the complete manual and reference
guide for the Apache Web server.
It also contains the basic web content (icons, default welcome messages,
etc) provided with Apache's HTTPD distribution.


%prep 
%setup -q -c -T -n %{name}-%{version} -a 0
%setup -q -c -T -D -n %{name}-%{version} -a 1
%setup -q -c -T -D -n %{name}-%{version} -a 2
%setup -q -c -T -D -n %{name}-%{version} -a 6

pushd %{aname}_%{apache_ver}
%patch0 -p0 -b .sslcfg
%patch1 -p1 -b .config
%patch3 -p0 -b .make
%patch5 -p1 -b .apachectl-init
%ifarch x86_64
%patch18 -p1 -b .apache-x86_64
%endif

#patch12 -p1 -b .dbmdb
%patch13 -p1 -b .compile
%patch15 -p0 -b .ap_getline


patch -p0 < ../libapreq-%{libapreq_ver}/patches/apache-1.3+apreq.patch
cp ../libapreq-%{libapreq_ver}/c/*.[ch] src/lib/apreq/
popd

pushd mod_ssl-%{mod_ssl_ver}-%{apache_ver}
%patch11 -p1 -b .openssl
%patch14 -p0 -b .stack
%ifarch x86_64
%patch16 -p1 -b .openssl-x86_64
%endif
popd

pushd mod_perl-%{mod_perl_ver}
%patch17 -p1 -b .mp1+perl5.14.diff
popd

# Substitute values to match the configuration.  The first two are
# for the default httpd.conf file, the rest is for the mod_ssl
# additions.
pushd %{aname}_%{apache_ver}
sed -e 's,@@ServerRoot@@,%{_sysconfdir}/%{pname},g' \
    -e 's,@@ContentRoot@@,%{contentdir},g' \
    -e 's,^DocumentRoot "@@ContentRoot@@",#DocumentRoot "%{_sysconfdir}/%{pname}/htdocs",g' \
    -e 's,^<Directory "@@ContentRoot@@/cgi-bin">,<Directory "%{contentdir}/cgi-bin">,g' \
    -e 's,^ServerName new.host.name,#ServerName new.host.name,g' \
    -e 's,^ServerAdmin you@your.address,#ServerAdmin you@your.address,g' \
    -e 's,^SSLCipherSuite,#SSLCipherSuite,g' \
    -e 's,^SSLLogLevel info,SSLLogLevel error,g' \
    -e 's,^SSLSessionCache         dbm:logs/ssl_scache,SSLSessionCache         shm:logs/ssl_scache(512000),g' \
    conf/httpd.conf-dist > conf/httpd.conf
popd

cp %{SOURCE5} .

#cp %{_tmppath}/rpm-tmp* /tmp/01prep.sh

%build
export CFLAGS="$RPM_OPT_FLAGS -fPIC $(pkg-config --cflags openssl)"
export LIBS="-lpthread"
export EAPI_MM=SYSTEM

###############################################
echo mod_perl ...
pushd mod_perl-%{mod_perl_ver}
  perl Makefile.PL CCFLAGS="$RPM_OPT_FLAGS -fPIC" \
    APACHE_SRC=../%{aname}_%{apache_ver}/src \
    DO_HTTPD=1 USE_APACI=1 PREP_HTTPD=1 EVERYTHING=1
  make %{?_smp_mflags}
  ## put mod_perl docs in a safe place ;-]~
  mkdir mod_perl-doc
  cp -a eg/ faq/ mod_perl-doc/
  cp {CREDITS,LICENSE,README,SUPPORT,STATUS,Changes,INSTALL*} mod_perl-doc/
  cp *.{pod,html,gif} mod_perl-doc/
  find mod_perl-doc -type f -exec chmod 644 {} \;
popd

###############################################
echo mod_ssl ...
export SSL_COMPAT=yes
export SSL_EXPERIMENTAL=yes
pushd mod_ssl-%{mod_ssl_ver}-%{apache_ver}
  ./configure --with-apache=../apache_%{apache_ver} \
    --with-mm=SYSTEM --force
popd

###############################################
echo apache ...
pushd %{aname}_%{apache_ver}
  ./configure \
 	--prefix=%{_prefix} \
 	--exec-prefix=%{_prefix} \
 	--bindir=%{_bindir} \
 	--sbindir=%{_sbindir} \
 	--mandir=%{_mandir} \
	--sysconfdir=%{_sysconfdir}/%{pname}/conf \
	--libexecdir=%{_libdir}/apache \
	--datadir=%{contentdir} \
	--iconsdir=%{contentdir}/icons \
	--htdocsdir=%{contentdir}/html \
	--manualdir=%{contentdir}/html/manual \
	--cgidir=%{contentdir}/cgi-bin \
 	--localstatedir=%{_localstatedir} \
	--runtimedir=%{_sysconfdir}/%{pname}/run \
	--logfiledir=logs \
	--proxycachedir=%{_localstatedir}/cache/%{pname} \
	--with-perl=%{__perl} \
	--enable-rule=EAPI \
	--enable-rule=SSL_COMPAT \
	--enable-rule=SSL_EXPERIMENTAL \
	--disable-rule=SSL_VENDOR \
	--disable-rule=WANTHSREGEX \
	--disable-rule=EXPAT \
	%{?_with_backtrace:--activate-module=src/modules/experimental/mod_backtrace.c} \
	%{?_with_whatkilledus:--activate-module=src/modules/experimental/mod_whatkilledus.c} \
	--activate-module=src/modules/perl/libperl.a \
	--enable-module=auth_dbm \
	--enable-module=ssl \
	--enable-module=all \
	--enable-shared=max \
	--disable-shared=perl \
	--disable-shared=ssl \
	--disable-module=example \
	--disable-module=auth_db \
	--without-execstrip \
	%{?_with_suexec:--enable-suexec --suexec-docroot=%{contentdir}} \
	%{?_with_suexec:--suexec-uidmin=300 --suexec-gidmin=300} \
	%{?_with_suexec:--suexec-umask=022 --suexec-caller=%{suexec_caller}}

  make %{?_smp_mflags}

popd
#cp %{_tmppath}/rpm-tmp* /tmp/02build.sh

%install
###############################################################################
### install basic apache stuff
pushd apache_%{apache_ver}
  make install root="$RPM_BUILD_ROOT"
popd

### rename 
mv $RPM_BUILD_ROOT%{_sbindir}/httpd $RPM_BUILD_ROOT%{_sbindir}/%{pname}

### install SYSV init stuff
mkdir -p $RPM_BUILD_ROOT%{_initrddir}
install -m755 %{SOURCE3} $RPM_BUILD_ROOT%{_initrddir}/%{pname}

### install log rotation stuff
mkdir -p $RPM_BUILD_ROOT%{_sysconfdir}/logrotate.d
install -m644 %{SOURCE4} $RPM_BUILD_ROOT%{_sysconfdir}/logrotate.d/apache

### default rootdir links
mkdir -p $RPM_BUILD_ROOT%{_localstatedir}/log/%{pname}
pushd $RPM_BUILD_ROOT%{_sysconfdir}/%{pname}
	ln -s %{_localstatedir}/log/%{pname} logs
	ln -s %{_libdir}/%{aname} modules
	ln -s %{_localstatedir}/run run
popd

### replace Apache's default config file with our patched version
install -m644 apache_%{apache_ver}/conf/httpd.conf \
	$RPM_BUILD_ROOT%{_sysconfdir}/%{pname}/conf/httpd.conf

# fix up apxs so that it doesn't think it's in the build root
perl -pi -e "s^$RPM_BUILD_ROOT^^g" $RPM_BUILD_ROOT%{_sbindir}/apxs

# fixup the documentation file naming
find $RPM_BUILD_ROOT%{contentdir} -name "*.html.html" | xargs rename .html.html .html

###############################################################################
### install mod_perl files
pushd mod_perl-%{mod_perl_ver}
  export PERL_INSTALL_ROOT=$RPM_BUILD_ROOT 
  make pure_install PREFIX=/usr INSTALLDIRS=vendor

  # convert man pages to UTF-8
  recode() {
    iconv -f "$2" -t utf-8 < "$1" > "${1}_"
    %{__mv} -f "${1}_" "$1"
  }
  pushd $RPM_BUILD_ROOT/usr/share/man/man3/
	for i in * ; do
		recode "${i}" iso-8859-1
	done
  popd

  # fix files mod
  find $RPM_BUILD_ROOT%{perl_vendorarch} -iname '*.pm' -exec chmod 0644 {} \;

  # bake web docs...
  mkdir -p $RPM_BUILD_ROOT%{contentdir}/html/manual/mod/mod_perl
  install -c -m 644 htdocs/manual/mod/mod_perl.html \
        $RPM_BUILD_ROOT%{contentdir}/html/manual/mod/mod_perl/
  make -C faq
  rm -f faq/pod2htm*
  install -m644 faq/*.html \
    $RPM_BUILD_ROOT%{contentdir}/html/manual/mod/mod_perl/

popd

# remove special perl files this is specific for rpms , already have in own .packlist
find $RPM_BUILD_ROOT%{perl_vendorarch}/.. -name perllocal.pod -o -name .packlist \
    -o -name '*.bs' | xargs -r -i rm -f {}

### ssl leftovers
# point to the right makefile.
ln -sf ../../../etc/pki/tls/certs/Makefile $RPM_BUILD_ROOT%{_sysconfdir}/%{pname}/conf
# create a prototype session cache
touch $RPM_BUILD_ROOT%{_localstatedir}/cache/ssl_gcache_data.{dir,pag,sem}

# drop shellbang from .exp files
for exp in $RPM_BUILD_ROOT%{perl_vendorarch}/auto/Apache/mod_perl.exp $RPM_BUILD_ROOT%{_libdir}/%{aname}/httpd.exp
do
	sed -i '/^#!/ d' $exp
done

#cp %{_tmppath}/rpm-tmp* /tmp/03install.sh

%post
/sbin/chkconfig --add %{pname}
/sbin/ldconfig

# safely build a test certificate
umask 077
if [ ! -f %{_sysconfdir}/%{pname}/conf/ssl.key/server.key ] ; then
openssl genrsa -rand /proc/apm:/proc/cpuinfo:/proc/dma:/proc/filesystems:/proc/interrupts:/proc/ioports:/proc/pci:/proc/rtc:/proc/uptime 1024 > %{_sysconfdir}/%{pname}/conf/ssl.key/server.key 2> /dev/null
fi

if [ ! -f %{_sysconfdir}/%{pname}/conf/ssl.crt/server.crt ] ; then
cat << EOF | openssl req -new -key %{_sysconfdir}/%{pname}/conf/ssl.key/server.key -x509 -days 365 -out %{_sysconfdir}/%{pname}/conf/ssl.crt/server.crt 2>/dev/null
--
SomeState
SomeCity
SomeOrganization
SomeOrganizationalUnit
localhost.localdomain
root@localhost.localdomain
EOF
fi

# safely add .htm to mime types if it is not already there
[ -f %{_sysconfdir}/mime.types ] || exit 0
TEMPTYPES=`/bin/mktemp /tmp/mimetypes.XXXXXX`
[ -z "$TEMPTYPES" ] && {
  echo "could not make temporary file, htm not added to %{_sysconfdir}/mime.types" >&2
  exit 1
}
( grep -v "^text/html"  %{_sysconfdir}/mime.types
  types=$(grep "^text/html" %{_sysconfdir}/mime.types | cut -f2-)
  echo -en "text/html\t\t\t"
  for val in $types ; do
      if [ "$val" = "htm" ] ; then
          continue
      fi
      echo -n "$val "
  done
  echo "htm"
) > $TEMPTYPES
cat $TEMPTYPES > %{_sysconfdir}/mime.types && /bin/rm -f $TEMPTYPES

cp %{_tmppath}/rpm-tmp* /tmp/04post.sh

%pre
# Add the "apache" user
/usr/sbin/useradd -c "Apache" -u 48 \
	-s /sbin/nologin -r -d "%{contentdir}" apache 2> /dev/null || :

%preun
if [ $1 = 0 ]; then
	if [ -f /var/lock/subsys/%{pname} ]; then
		%{_initrddir}/%{pname} stop
	fi
	if [ -f %{_initrddir}/%{pname} ]; then
		/sbin/chkconfig --del %{pname}
	fi
fi

%files
%defattr(-,root,root)
%dir %{_sysconfdir}/%{pname}
%dir %{_sysconfdir}/%{pname}/conf
%config(noreplace) %{_sysconfdir}/%{pname}/conf/*.conf
%config(noreplace) %{_sysconfdir}/%{pname}/conf/Makefile
%config(noreplace) %{_sysconfdir}/%{pname}/conf/magic
%config(noreplace) %{_sysconfdir}/%{pname}/conf/mime.types
%config(noreplace) %{_sysconfdir}/logrotate.d/*
%config(noreplace) %{_sysconfdir}/%{pname}/conf/ssl.*
%doc %{_sysconfdir}/%{pname}/conf/*.default
%attr(755,root,root) %{_initrddir}/*
%{_sysconfdir}/%{pname}/logs
%{_sysconfdir}/%{pname}/modules
%{_sysconfdir}/%{pname}/run
%{_libdir}/%{aname}
%{perl_vendorarch}/Apache
%{perl_vendorarch}/Bundle
%{perl_vendorarch}/*.pm
%{perl_vendorarch}/*.PL
%dir %{perl_vendorarch}/auto/Apache
%{perl_vendorarch}/auto/Apache/Leak
%{perl_vendorarch}/auto/Apache/Symbol
%{perl_vendorarch}/auto/Apache/mod_perl.exp
%{perl_vendorarch}/auto/Apache/typemap
%attr(0755,root,root) %{_bindir}/*
%attr(0755,root,root) %{_sbindir}/ab
%attr(0755,root,root) %{_sbindir}/apachectl
%attr(0755,root,root) %{_sbindir}/httpd13
%attr(0755,root,root) %{_sbindir}/logresolve
%attr(0755,root,root) %{_sbindir}/rotatelogs
%{?_with_suexec:%attr(4710,root,%{suexec_caller}) %{_sbindir}/suexec}
%{_mandir}/man1*/*
%{_mandir}/man8/ab.8*
%{_mandir}/man8/apachectl.8*
%{_mandir}/man8/httpd.8*
%{_mandir}/man8/logresolve.8*
%{_mandir}/man8/rotatelogs.8*
%{?_with_suexec:%{_mandir}/man8/suexec.8*}
%attr(0755,apache,root) %dir %{_localstatedir}/cache/%{pname}
%attr(0640,apache,root) %{_localstatedir}/cache/ssl_*
%attr(0750,root,apache) %dir %{_localstatedir}/log/%{pname}


%files devel
%defattr(-,root,root)
%{_includedir}
%attr(0755,root,root) %{_sbindir}/apxs
%{_mandir}/man8/apxs.8*
%doc %{perl_vendorarch}/*.pod
%{perl_vendorarch}/auto/Apache/include

%files manual
%defattr(-,root,root)
%doc apache_%{apache_ver}/cgi-bin
%dir %{contentdir}
%dir %{contentdir}/cgi-bin
%config(noreplace) %{contentdir}/cgi-bin/*
%dir %{contentdir}/html
%config(noreplace) %{contentdir}/html/*.html*
%config(noreplace) %{contentdir}/html/*.gif
%dir %{contentdir}/icons
%dir %{contentdir}/icons/small
%config(noreplace) %{contentdir}/icons/*.*
%config(noreplace) %{contentdir}/icons/small/*.*
%doc %{contentdir}/icons/README

%doc apache_%{apache_ver}/{ABOUT_APACHE,LICENSE*,NOTICE,README*,cgi-bin}
%doc apache_%{apache_ver}/src/{CHANGES,README}*
%doc mod_ssl-%{mod_ssl_ver}-%{apache_ver}/README.*
%doc mod_ssl-%{mod_ssl_ver}-%{apache_ver}/NEWS
%doc mod_perl-%{mod_perl_ver}/mod_perl-doc
%doc SSL-Certificate-Creation

%doc %{contentdir}/html/manual
%exclude %{contentdir}/html/manual/mod/mod_ssl/ssl_cover.wml
#man3 conflicts with mod_perl2
/usr/share/man/man3*/*


%changelog
* Sun May 13 2012 Sérgio Basto <sergio@serjux.com> - 1.3.42-4
- Many improvements on defaults directories 
- Separate libapreq in other package, to compile libapreq is need other sources installed. 
- more cleanups.

* Wed Nov 16 2011 Sérgio Basto <sergio@serjux.com>
- build for F16
- mp1+perl5.14.diff (mod_perl1 + perl5.14)
- many improvents.

* Sat Oct 29 2011 Sérgio Basto <sergio@serjux.com>
- mock build add many buildrequires
- many improvemts on confs 

* Tue Oct 16 2007 Sérgio Basto <sergio@serjux.com>
- UNDROPPED CONFIGURATION COMPLETELY: rpm it suposed do all alone.
- rename httpd to http13 to work independently of apache 2.2
- add patch to Makefile.tmp, resolve all problems at once
- change server port number to run out of the box.
- Update link to certs/Makefile.

* Tue Sep 11 2007 Marius FERARU <altblue@n0i.net> - 1.3.39-1.n0i.23.MPSSL
- apache 1.3.39
- mod_ssl 2.8.29

* Mon Apr 02 2007 Marius FERARU <altblue@n0i.net> - 1.3.37-3.n0i.22.MPSSL
- mod_perl 1.30
- initscript: use a "$pidfile" variable for all operations
- initscript: added a dummy "alias" for "reload" (will do a 'restart'!)
- initscript: added missing "fullstatus" option (will run through "apachectl")
- dropped shellbang from .exp files
- dropped 2 explicit provides (mod_perl and Apache::Constants)

* Fri Sep 08 2006 Marius FERARU <altblue@n0i.net> - 1.3.37-2.n0i.21.MPSSL
- BR: gdbm-devel, db4-devel

* Mon Aug 21 2006 Marius FERARU <altblue@n0i.net> - 1.3.37-1.n0i.20.MPSSL
- apache 1.3.37
- mod_ssl 2.8.28
- Dist macro
- URL update
- updated description
- spec cleanups
- use "--with backtrace" to activate "mod_backtrace"
- use "--with whatkilledus" to activate "mod_whatkilledus"
- use "--with suexec" to activate suexec functionality
- moved default web content into documentation package

* Tue Jun 06 2006 Marius FERARU <altblue@n0i.net> - 1.3.36-2.n0i.19.MPSSL
- changed "runtimedir" and "logfiledir" to relative paths,
  letting users run apache on their own

* Tue Jun 06 2006 Marius FERARU <altblue@n0i.net> - 1.3.36-1.n0i.19.MPSSL
- apache 1.3.36
- mod_ssl version 2.8.27
- spec cleanups

* Mon Mar 13 2006 Marius FERARU <altblue@n0i.net> - 1.3.34-2.n0i.18.MPSSL
- rebuild

* Thu Nov 24 2005 Marius FERARU <altblue@n0i.net> 1.3.34-1.n0i.17.MPSSL
- apache 1.3.34
- mod_ssl version 2.8.25

* Tue Sep 20 2005 Marius FERARU <altblue@n0i.net> 1.3.33-5.n0i.16.MPSSL
- mod_ssl version 2.8.24

* Fri Sep 02 2005 Marius FERARU <altblue@n0i.net> 1.3.33-4.n0i.15.MPSSL
- rebuild
- dropped more requirements (which Fedora considers to "always have")

* Sat Jul 23 2005 Marius FERARU <altblue@n0i.net> 1.3.33-3.n0i.14.MPSSL
- dropped Epoch
- changed Summary and Description
- rebuild (perl 5.8.7)

* Tue Jan 04 2005 Marius FERARU <altblue@n0i.net> 1.3.33-2.n0i.13.MPSSL
- libapreq version 1.33

* Mon Dec 06 2004 Marius FERARU <altblue@n0i.net> 1.3.33-1.n0i.12.MPSSL
- apache version 1.3.33
- mod_ssl version 2.8.22
- description update

* Tue Aug 17 2004 Marius FERARU <altblue@n0i.net> 1.3.31-5.n0i.11.MPSSL
- mod_ssl version 2.8.19

* Thu Jul 15 2004 Marius FERARU <altblue@n0i.net> 1.3.31-4.n0i.10.MPSSL
- mod_ssl version 2.8.18

* Tue Jul 13 2004 Marius FERARU <altblue@n0i.net> 1.3.31-3.n0i.9.MPSSL
- tweaked rotatelog's build: drop linking with apache libs

* Tue Jul 13 2004 Marius FERARU <altblue@n0i.net> 1.3.31-2.n0i.8.MPSSL
- applied some fixing patches from current CVS version

* Thu May 13 2004 Marius FERARU <altblue@n0i.net> 1.3.31-1.n0i.7.MPSSL
- apache version 1.3.31
- mod_ssl version 2.8.17
- updated apxs patch
- slight spec tweaks
- enabled backtrace experimental module
- updated config patch

* Fri Apr 30 2004 Marius Feraru <altblue@n0i.net> 1.3.29-6.n0i.6.MPSSL
- automatic rebuild

* Thu Apr 22 2004 Marius FERARU <altblue@n0i.net> 1.3.29-5.n0i.5.MPSSL
- rebuild (perl 5.8.4)

* Tue Feb 10 2004 Marius FERARU <altblue@n0i.net> 1.3.29-4.n0i.4.MPSSL
- fixed the shameful bugs from my httpd.init script

* Fri Jan 23 2004 Marius FERARU <altblue@n0i.net> 1.3.29-3.n0i.3.MPSSL
- rebuild (perl 5.8.3)

* Fri Jan 16 2004 Marius FERARU <altblue@n0i.net> 1.3.29-2.n0i.2.MPSSL
- rebuilt on perl 5.8.2 / Fedora 1 Devel (tobe FC2)
- finally clearly enabled modperl.c in apache_1.3.23-config.patch
as many helpless people seem to use this dumb default configuration file :(
- also updated the same patch to properly define SSL too :)
- added a lame "MPSSL" extra tag in release to make people understand this
is a !SPECIAL! apache + mod_perl + mod_ssl + libapreq package suite!!!
- updated init script to do "real" server shutdown (in squid style) and to NOT
shutdown all the apache servers, just the one started with /var/run/httpd.pid
- added USE_MODULEARGS=[yes/no] and SHUTDOWN_TIMEOUT=[seconds] configuration
parameters to init script

* Thu Nov 13 2003 Marius FERARU <altblue@n0i.net> 1.3.29-1.n0i.1
- apache 1.3.29
- modssl 2.8.16
- dropped zombie patch
- dropped fderr patch
- dropped for good thttpd conflict note as THERE IS NO CONFLICT!!! In fact
we really use them both for long time without a problem :))
- added more Prereq stuff
- more Fedora style spec updates

* Thu Oct 16 2003 Marius FERARU <altblue@n0i.net> 1.3.28-2.n0i
- mod_perl 1.29
- libapreq 1.3
- replaced ALL direct 'etc' occurences to macros (some for other stuff)
- perl %%files are now more properly quested.
- switched krb5-config to pkg-config
- switched textutils to coreutils
- using mm 1.3
- disabled internal expat linking
- added the 'zombie' patch
- added the 'file descriptors are erroneously closed' patch

* Mon Jul 28 2003 Marius FERARU <altblue.net> 1.3.28-1.n0i
- mod_perl 1.28
- added builtin libapreq

* Fri Jul 25 2003 Marius FERARU <altblue@n0i.net> 1.3.28-0.n0i
- apache version 1.3.28
- mod_ssl version 2.8.15
- switched the old dbm-gdbm patch with a more elegant one (apache_1.3.27-db);
yet, more tests are to be done on other systems before dropping the old one
from our src.rpm
- disabled suexec SSL env support patch as Apache ppl changed their code heavily
and I do not yet have time to update this patch
- moved mod_perl header files into apache-devel (are they needed by someone?!)
- dropped using RPM_SOURCE_DIR/<filename> stuff.
- disabled auth_db module (db4 API changes?!)

* Mon Apr 21 2003 Marius FERARU <altblue@n0i.net> 1.3.27-2.n0i
- automatic rebuild on RHL9

* Wed Mar 26 2003 Marius FERARU <altblue@n0i.net> 1.3.27-1.n0i
- mod_ssl version 2.8.14
- dropped thttpd conflict note as THERE IS NO CONFLICT!!! In fact we really
use them both :))

* Fri Oct 18 2002 Marius Feraru <altblue@n0i.net>
- apache version 1.3.27
- mod_ssl version 2.8.11
- eliminated db4 patch
- disabled thttpd conflict flag

* Tue Sep 24 2002 Marius Feraru <altblue@n0i.net>
- automatic rebuild (to conform with the openssl update)

* Wed Sep  4 2002 Marius Feraru <altblue@n0i.net>
- some spec cleanups (rpm 4.1.x compatibility)

* Tue Jul 23 2002 Marius FERARU <altblue@n0i.net>
- apache 1.3.26
- mod_perl 1.27
- mod_ssl 2.8.10
- lots of new tweaks to the spec file (hopefully it will be easier now for others to
rebuild this package =] )


* Sat Sep 1 2001 Marius FERARU <altblue@n0i.net>
- updated apache to 1.3.22
- reparsed and tweaked all RedHat patches
- lots of spec file tweaks: optimisations,
  file location/integration/modes fixes...

* Sat Sep 1 2001 Marius FERARU <altblue@n0i.net>
- updated mod_perl to version 1.26
- based on apache-1.3.20-15.src.rpm from Red Hat RawHide
- used apache_modperl-1.3.19-1.24-1.src.rpm from
  perl.apache.org as example spec.
