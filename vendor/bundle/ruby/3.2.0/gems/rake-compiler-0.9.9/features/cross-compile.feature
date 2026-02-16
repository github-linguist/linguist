Feature: Cross-compile C extensions

  In order to avoid bitching from Windows users
  As a Ruby developer on Linux
  I want some rake tasks that take away the pain of compilation

  Scenario: compile single extension
    Given that all my source files are in place
    And I'm running a POSIX operating system
    And I've installed cross compile toolchain
    When rake task 'cross compile' is invoked
    Then rake task 'cross compile' succeeded
    And binaries for platform 'i386-mingw32' get generated

  Scenario: compile single extension to multiple versions
    Given that all my source files are in place
    And I'm running a POSIX operating system
    And I've installed cross compile toolchain
    When rake task 'cross compile RUBY_CC_VERSION=1.8.7:1.9.3:2.0.0' is invoked
    Then rake task 'cross compile RUBY_CC_VERSION=1.8.7:1.9.3:2.0.0' succeeded
    And binaries for platform 'i386-mingw32' version '1.8' get copied
    And binaries for platform 'i386-mingw32' version '1.9' get copied
    And binaries for platform 'i386-mingw32' version '2.0' get copied
