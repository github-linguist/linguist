<?hh
/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

final class TypehintViolationException extends Exception {
}

function setup_errors(): void {
  set_error_handler('handle_error', E_ALL);
}

/**
 * I want to turn failed typehints into exceptions so that I can handle them in
 * my example code
 */
function handle_error(
  $errno,
  $errstr,
  $errfile,
  $errline,
  $errcontext = array(),
  $errtrace = array(),
): bool {
  if (E_RECOVERABLE_ERROR == $errno) {
    // Transform typehint failures into an exception.
    if (strpos($errstr, 'must be an instance of ') !== false) {
      throw new TypehintViolationException($errstr);
    }
    // Transform nullable type violations to exceptions.
    if ((strpos($errstr, 'must be of type ?') !== false) &&
        (strpos($errstr, 'Value returned from') === false)) {
      throw new TypehintViolationException($errstr);
    }
  }
  return false;
}
