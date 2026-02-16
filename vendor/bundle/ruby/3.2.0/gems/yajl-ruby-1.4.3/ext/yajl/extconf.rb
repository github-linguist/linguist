require 'mkmf'
require 'rbconfig'

$CFLAGS << ' -Wall -funroll-loops -Wno-declaration-after-statement'
$CFLAGS << ' -Werror-implicit-function-declaration -Wextra -O0 -ggdb3' if ENV['DEBUG']

if ENV['SANITIZE']
  $CFLAGS << ' -fsanitize=address'
  $LDFLAGS << ' -fsanitize=address'
end

create_makefile('yajl/yajl')
