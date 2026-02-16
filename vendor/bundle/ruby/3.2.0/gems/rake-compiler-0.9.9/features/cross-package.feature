Feature: Generate Windows gems from Linux

  In order to keep sanity in the Ruby world
  As a Gem developer on Linux
  I want more rake magic that turns monotony into joy.

  Scenario: package a gem for Windows
    Given that my gem source is all in place
    And I'm running a POSIX operating system
    And I've installed cross compile toolchain
    And I've already successfully executed rake task 'cross compile'
    When rake task 'cross native gem' is invoked
    Then rake task 'cross native gem' succeeded
    And gem for platform 'x86-mingw32' get generated
