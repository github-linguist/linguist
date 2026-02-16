Feature: JCompile Java extensions

  In order to avoid bitching from Enterprise users
  As a Ruby developer
  I want some rake tasks that take away the pain of compilation

  @java
  Scenario: Compile single Java extension (with default Rake)
    Given that all my Java source files are in place
    And I've installed the Java Development Kit
    When rake task 'java compile' is invoked
    Then rake task 'java compile' succeeded
    And binaries for platform 'java' get generated

  @java
  Scenario: Compile single Java extension (with Rake on JRuby)
    Given that all my Java source files are in place
    And I've installed the Java Development Kit
    When I've installed JRuby
    When rake task 'java compile' is invoked on JRuby
    Then rake task 'java compile' succeeded
    And binaries for platform 'java' get generated
