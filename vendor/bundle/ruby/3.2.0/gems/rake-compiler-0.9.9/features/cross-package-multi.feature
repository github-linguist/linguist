Feature: Generate multiple Windows gems from Linux

  In order to keep compatibility with versions of Ruby on Windows
  As a Gem developer on Linux
  I want to build binary gems for One-Click Installer (old and new versions)

  Scenario: package multiple gems for Windows
    Given that my gem source is all in place to target two platforms
    And I'm running a POSIX operating system
    And I've installed cross compile toolchain
    And I've already successfully executed rake task 'cross compile'
    When rake task 'cross native gem' is invoked
    Then rake task 'cross native gem' succeeded
    And gem for platform 'x86-mswin32-60' get generated
    And gem for platform 'x86-mingw32' get generated
