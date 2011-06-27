#!/bin/sh
#| -*- scheme -*-
exec racket -um "$0" "$@"
|#

(require racket/file racket/path racket/list racket/string
         (for-syntax racket/base))
