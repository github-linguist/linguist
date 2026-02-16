Feature: Compile C code into Ruby extensions.

  In order to automate compilation process.
  As a Gem developer.
  I want rake tasks compile source code for me.

  Scenario: compile single extension
    Given a safe project directory
    And a extension named 'extension_one'
    And 'tmp' folder is deleted
    When rake task 'compile' is invoked
    Then rake task 'compile' succeeded
    And binary extension 'extension_one' do exist in 'lib'
    And 'tmp' folder is created

  Scenario: compile an extension with extra options
    Given a safe project directory
    And a extension named 'extension_one'
    And 'tmp' folder is deleted
    When rake task 'compile -- --with-opt-dir=/opt/local' is invoked
    Then rake task 'compile -- --with-opt-dir=/opt/local' succeeded
    And output of rake task 'compile -- --with-opt-dir=/opt/local' contains /with-opt-dir/

  Scenario: not recompile unmodified extension
    Given a safe project directory
    And a extension named 'extension_one'
    And I've already successfully executed rake task 'compile'
    And not changed any file since
    When rake task 'compile' is invoked
    Then rake task 'compile' succeeded
    And output of rake task 'compile' do not contain /gcc|cl/

  Scenario: recompile extension when source is modified
    Given a safe project directory
    And a extension named 'extension_one'
    And I've already successfully executed rake task 'compile'
    When touching 'source.c' file of extension 'extension_one'
    And rake task 'compile' is invoked
    Then rake task 'compile' succeeded
    And output of rake task 'compile' contains /extension_one/

  Scenario: compile multiple extensions
    Given a safe project directory
    And a extension named 'extension_one'
    And a extension named 'extension_two'
    And 'tmp' folder is deleted
    When rake task 'compile' is invoked
    Then rake task 'compile' succeeded
    And binary extension 'extension_one' do exist in 'lib'
    And binary extension 'extension_two' do exist in 'lib'
    And 'tmp' folder is created

  Scenario: compile one extension instead of all present
    Given a safe project directory
    And a extension named 'extension_one'
    And a extension named 'extension_two'
    When rake task 'compile:extension_one' is invoked
    Then rake task 'compile:extension_one' succeeded
    And output of rake task 'compile:extension_one' do not contain /extension_two/
    And binary extension 'extension_one' do exist in 'lib'
    And binary extension 'extension_two' do not exist in 'lib'

  Scenario: removing temporary files
    Given a safe project directory
    And a extension named 'extension_one'
    And I've already successfully executed rake task 'compile'
    When rake task 'clean' is invoked
    Then rake task 'clean' succeeded
    And binary extension 'extension_one' do exist in 'lib'
    And no left over from 'extension_one' remains in 'tmp'

  Scenario: clobbering binary and temporary files
    Given a safe project directory
    And a extension named 'extension_one'
    And I've already successfully executed rake task 'compile'
    When rake task 'clobber' is invoked
    Then rake task 'clobber' succeeded
    And binary extension 'extension_one' do not exist in 'lib'
    And 'tmp' folder do not exist
