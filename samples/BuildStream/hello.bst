kind: manual
description: |

  Building manually

# Depend on the base system
depends:
- base.bst

# Stage the files/src directory for building
sources:
  - kind: local
    path: files/src

# Now configure the commands to run
config:

  build-commands:
  - make PREFIX="%{prefix}"

  install-commands:
  - make -j1 PREFIX="%{prefix}" DESTDIR="%{install-root}" install
