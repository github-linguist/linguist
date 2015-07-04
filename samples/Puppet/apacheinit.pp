# Class: apache
#
# This class installs Apache
#
# Parameters:
#
# Actions:
#   - Install Apache
#   - Manage Apache service
#
# Requires:
#
# Sample Usage:
#
class apache (
  $apache_name            = $::apache::params::apache_name,
  $service_name           = $::apache::params::service_name,
  $default_mods           = true,
  $default_vhost          = true,
  $default_charset        = undef,
  $default_confd_files    = true,
  $default_ssl_vhost      = false,
  $default_ssl_cert       = $::apache::params::default_ssl_cert,
  $default_ssl_key        = $::apache::params::default_ssl_key,
  $default_ssl_chain      = undef,
  $default_ssl_ca         = undef,
  $default_ssl_crl_path   = undef,
  $default_ssl_crl        = undef,
  $default_ssl_crl_check  = undef,
  $default_type           = 'none',
  $ip                     = undef,
  $service_enable         = true,
  $service_manage         = true,
  $service_ensure         = 'running',
  $purge_configs          = true,
  $purge_vhost_dir        = undef,
  $purge_vdir             = false,
  $serveradmin            = 'root@localhost',
  $sendfile               = 'On',
  $error_documents        = false,
  $timeout                = '120',
  $httpd_dir              = $::apache::params::httpd_dir,
  $server_root            = $::apache::params::server_root,
  $conf_dir               = $::apache::params::conf_dir,
  $confd_dir              = $::apache::params::confd_dir,
  $vhost_dir              = $::apache::params::vhost_dir,
  $vhost_enable_dir       = $::apache::params::vhost_enable_dir,
  $mod_dir                = $::apache::params::mod_dir,
  $mod_enable_dir         = $::apache::params::mod_enable_dir,
  $mpm_module             = $::apache::params::mpm_module,
  $lib_path               = $::apache::params::lib_path,
  $conf_template          = $::apache::params::conf_template,
  $servername             = $::apache::params::servername,
  $manage_user            = true,
  $manage_group           = true,
  $user                   = $::apache::params::user,
  $group                  = $::apache::params::group,
  $keepalive              = $::apache::params::keepalive,
  $keepalive_timeout      = $::apache::params::keepalive_timeout,
  $max_keepalive_requests = $::apache::params::max_keepalive_requests,
  $logroot                = $::apache::params::logroot,
  $logroot_mode           = $::apache::params::logroot_mode,
  $log_level              = $::apache::params::log_level,
  $log_formats            = {},
  $ports_file             = $::apache::params::ports_file,
  $docroot                = $::apache::params::docroot,
  $apache_version         = $::apache::version::default,
  $server_tokens          = 'OS',
  $server_signature       = 'On',
  $trace_enable           = 'On',
  $allow_encoded_slashes  = undef,
  $package_ensure         = 'installed',
  $use_optional_includes  = $::apache::params::use_optional_includes,
) inherits ::apache::params {
  validate_bool($default_vhost)
  validate_bool($default_ssl_vhost)
  validate_bool($default_confd_files)
  # true/false is sufficient for both ensure and enable
  validate_bool($service_enable)
  validate_bool($service_manage)
  validate_bool($use_optional_includes)

  $valid_mpms_re = $apache_version ? {
    '2.4'   => '(event|itk|peruser|prefork|worker)',
    default => '(event|itk|prefork|worker)'
  }

  if $mpm_module {
    validate_re($mpm_module, $valid_mpms_re)
  }

  if $allow_encoded_slashes {
    validate_re($allow_encoded_slashes, '(^on$|^off$|^nodecode$)', "${allow_encoded_slashes} is not permitted for allow_encoded_slashes. Allowed values are 'on', 'off' or 'nodecode'.")
  }

  # NOTE: on FreeBSD it's mpm module's responsibility to install httpd package.
  # NOTE: the same strategy may be introduced for other OSes. For this, you
  # should delete the 'if' block below and modify all MPM modules' manifests
  # such that they include apache::package class (currently event.pp, itk.pp,
  # peruser.pp, prefork.pp, worker.pp).
  if $::osfamily != 'FreeBSD' {
    package { 'httpd':
      ensure => $package_ensure,
      name   => $apache_name,
      notify => Class['Apache::Service'],
    }
  }
  validate_re($sendfile, [ '^[oO]n$' , '^[oO]ff$' ])

  # declare the web server user and group
  # Note: requiring the package means the package ought to create them and not puppet
  validate_bool($manage_user)
  if $manage_user {
    user { $user:
      ensure  => present,
      gid     => $group,
      require => Package['httpd'],
    }
  }
  validate_bool($manage_group)
  if $manage_group {
    group { $group:
      ensure  => present,
      require => Package['httpd']
    }
  }

  validate_apache_log_level($log_level)

  class { '::apache::service':
    service_name   => $service_name,
    service_enable => $service_enable,
    service_manage => $service_manage,
    service_ensure => $service_ensure,
  }

  # Deprecated backwards-compatibility
  if $purge_vdir {
    warning('Class[\'apache\'] parameter purge_vdir is deprecated in favor of purge_configs')
    $purge_confd = $purge_vdir
  } else {
    $purge_confd = $purge_configs
  }

  # Set purge vhostd appropriately
  if $purge_vhost_dir == undef {
    $purge_vhostd = $purge_confd
  } else {
    $purge_vhostd = $purge_vhost_dir
  }

  Exec {
    path => '/bin:/sbin:/usr/bin:/usr/sbin',
  }

  exec { "mkdir ${confd_dir}":
    creates => $confd_dir,
    require => Package['httpd'],
  }
  file { $confd_dir:
    ensure  => directory,
    recurse => true,
    purge   => $purge_confd,
    notify  => Class['Apache::Service'],
    require => Package['httpd'],
  }

  if ! defined(File[$mod_dir]) {
    exec { "mkdir ${mod_dir}":
      creates => $mod_dir,
      require => Package['httpd'],
    }
    # Don't purge available modules if an enable dir is used
    $purge_mod_dir = $purge_configs and !$mod_enable_dir
    file { $mod_dir:
      ensure  => directory,
      recurse => true,
      purge   => $purge_mod_dir,
      notify  => Class['Apache::Service'],
      require => Package['httpd'],
    }
  }

  if $mod_enable_dir and ! defined(File[$mod_enable_dir]) {
    $mod_load_dir = $mod_enable_dir
    exec { "mkdir ${mod_enable_dir}":
      creates => $mod_enable_dir,
      require => Package['httpd'],
    }
    file { $mod_enable_dir:
      ensure  => directory,
      recurse => true,
      purge   => $purge_configs,
      notify  => Class['Apache::Service'],
      require => Package['httpd'],
    }
  } else {
    $mod_load_dir = $mod_dir
  }

  if ! defined(File[$vhost_dir]) {
    exec { "mkdir ${vhost_dir}":
      creates => $vhost_dir,
      require => Package['httpd'],
    }
    file { $vhost_dir:
      ensure  => directory,
      recurse => true,
      purge   => $purge_vhostd,
      notify  => Class['Apache::Service'],
      require => Package['httpd'],
    }
  }

  if $vhost_enable_dir and ! defined(File[$vhost_enable_dir]) {
    $vhost_load_dir = $vhost_enable_dir
    exec { "mkdir ${vhost_load_dir}":
      creates => $vhost_load_dir,
      require => Package['httpd'],
    }
    file { $vhost_enable_dir:
      ensure  => directory,
      recurse => true,
      purge   => $purge_vhostd,
      notify  => Class['Apache::Service'],
      require => Package['httpd'],
    }
  } else {
    $vhost_load_dir = $vhost_dir
  }

  concat { $ports_file:
    owner   => 'root',
    group   => $::apache::params::root_group,
    mode    => '0644',
    notify  => Class['Apache::Service'],
    require => Package['httpd'],
  }
  concat::fragment { 'Apache ports header':
    ensure  => present,
    target  => $ports_file,
    content => template('apache/ports_header.erb')
  }

  if $::apache::conf_dir and $::apache::params::conf_file {
    case $::osfamily {
      'debian': {
        $pidfile              = "\${APACHE_PID_FILE}"
        $error_log            = 'error.log'
        $scriptalias          = '/usr/lib/cgi-bin'
        $access_log_file      = 'access.log'
      }
      'redhat': {
        $pidfile              = 'run/httpd.pid'
        $error_log            = 'error_log'
        $scriptalias          = '/var/www/cgi-bin'
        $access_log_file      = 'access_log'
      }
      'freebsd': {
        $pidfile              = '/var/run/httpd.pid'
        $error_log            = 'httpd-error.log'
        $scriptalias          = '/usr/local/www/apache24/cgi-bin'
        $access_log_file      = 'httpd-access.log'
      } 'gentoo': {
        $pidfile              = '/run/apache2.pid'
        $error_log            = 'error.log'
        $error_documents_path = '/usr/share/apache2/error'
        $scriptalias          = '/var/www/localhost/cgi-bin'
        $access_log_file      = 'access.log'

        ::portage::makeconf { 'apache2_modules':
          content => $default_mods,
        }
        file { [
          '/etc/apache2/modules.d/.keep_www-servers_apache-2',
          '/etc/apache2/vhosts.d/.keep_www-servers_apache-2'
        ]:
          ensure  => absent,
          require => Package['httpd'],
        }
      }
      default: {
        fail("Unsupported osfamily ${::osfamily}")
      }
    }

    $apxs_workaround = $::osfamily ? {
      'freebsd' => true,
      default   => false
    }

    # Template uses:
    # - $pidfile
    # - $user
    # - $group
    # - $logroot
    # - $error_log
    # - $sendfile
    # - $mod_dir
    # - $ports_file
    # - $confd_dir
    # - $vhost_dir
    # - $error_documents
    # - $error_documents_path
    # - $apxs_workaround
    # - $keepalive
    # - $keepalive_timeout
    # - $max_keepalive_requests
    # - $server_root
    # - $server_tokens
    # - $server_signature
    # - $trace_enable
    file { "${::apache::conf_dir}/${::apache::params::conf_file}":
      ensure  => file,
      content => template($conf_template),
      notify  => Class['Apache::Service'],
      require => Package['httpd'],
    }

    # preserve back-wards compatibility to the times when default_mods was
    # only a boolean value. Now it can be an array (too)
    if is_array($default_mods) {
      class { '::apache::default_mods':
        all  => false,
        mods => $default_mods,
      }
    } else {
      class { '::apache::default_mods':
        all => $default_mods,
      }
    }
    class { '::apache::default_confd_files':
      all => $default_confd_files
    }
    if $mpm_module {
      class { "::apache::mod::${mpm_module}": }
    }

    $default_vhost_ensure = $default_vhost ? {
      true  => 'present',
      false => 'absent'
    }
    $default_ssl_vhost_ensure = $default_ssl_vhost ? {
      true  => 'present',
      false => 'absent'
    }

    ::apache::vhost { 'default':
      ensure          => $default_vhost_ensure,
      port            => 80,
      docroot         => $docroot,
      scriptalias     => $scriptalias,
      serveradmin     => $serveradmin,
      access_log_file => $access_log_file,
      priority        => '15',
      ip              => $ip,
      logroot_mode    => $logroot_mode,
      manage_docroot  => $default_vhost,
    }
    $ssl_access_log_file = $::osfamily ? {
      'freebsd' => $access_log_file,
      default   => "ssl_${access_log_file}",
    }
    ::apache::vhost { 'default-ssl':
      ensure          => $default_ssl_vhost_ensure,
      port            => 443,
      ssl             => true,
      docroot         => $docroot,
      scriptalias     => $scriptalias,
      serveradmin     => $serveradmin,
      access_log_file => $ssl_access_log_file,
      priority        => '15',
      ip              => $ip,
      logroot_mode    => $logroot_mode,
      manage_docroot  => $default_ssl_vhost,
    }
  }
}
