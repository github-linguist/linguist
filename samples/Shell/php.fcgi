#!/bin/sh
# you can change the PHP version here.
version="RB_PHP_VERSION_X_Y_Z"

# php.ini file location
PHPRC=/usr/local/php/phpfarm/inst/php-${version}/lib/php.ini
export PHPRC

PHP_FCGI_CHILDREN=3
export PHP_FCGI_CHILDREN

PHP_FCGI_MAX_REQUESTS=5000
export PHP_FCGI_MAX_REQUESTS

# which php-cgi binary to execute
exec /usr/local/php/inst/php-${version}/bin/php-cgi
