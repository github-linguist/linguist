class apache::params{
  $user  = 'www-data'
  $group = 'www-data'

  case $operatingsystem {
    "centos": {
       $apache_name = httpd
       $ssl_package = mod_ssl
       $apache_dev  = httpd-devel
    }
    "ubuntu": {
       $apache_name = apache2
       $ssl_package = apache-ssl
       $apache_dev  = [ libaprutil1-dev, libapr1-dev, apache2-prefork-dev ]
    }
  }
}
