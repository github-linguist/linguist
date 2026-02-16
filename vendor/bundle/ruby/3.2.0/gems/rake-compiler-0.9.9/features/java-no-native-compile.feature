Feature: No native or cross compilation on JRuby

  In order to present a good user experience to users of rake-compiler
  As a user of JRuby
  I want to be warned that my platform does not provide any support for C Extensions
  I want to be be informed of this without rake-compiler blowing up in my face

  @java
  Scenario: Attempting to do a cross compilation while on JRuby (without prerequisites)
    Given that all my source files are in place
    And I'm running a POSIX operating system
    When rake task 'cross compile' is invoked on JRuby
    Then rake task 'cross compile' should fail
    And output of rake task 'cross compile' warns
      """
      WARNING: You're attempting to (cross-)compile C extensions from a platform
      (jruby) that does not support native extensions or mkmf.rb.
      """
    And output of rake task 'cross compile' contains /Don't know how to build task 'cross'/

  @java
  Scenario: Attempting to do a cross compilation while on JRuby (even with prerequisites)
    Given that all my source files are in place
    And I'm running a POSIX operating system
    And I've installed cross compile toolchain
    When rake task 'cross compile' is invoked on JRuby
    Then rake task 'cross compile' should fail
    And output of rake task 'cross compile' warns
      """
      WARNING: You're attempting to (cross-)compile C extensions from a platform
      (jruby) that does not support native extensions or mkmf.rb.
      """
    And output of rake task 'cross compile' contains /Don't know how to build task 'cross'/
