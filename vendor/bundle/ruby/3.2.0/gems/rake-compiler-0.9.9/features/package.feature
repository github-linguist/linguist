Feature: Distribute native extension with gems

  In order to avoid compiler toolchain requirement during installation
  As a Gem developer.
  I want rake tasks generate platform specific gems for me

  Scenario: generate pure ruby gem
    Given a safe project directory
    And a gem named 'my_project'
    And a extension named 'extension_one'
    And I've already successfully executed rake task 'compile'
    And 'pkg' folder is deleted
    When rake task 'gem' is invoked
    Then rake task 'gem' succeeded
    And 'pkg' folder is created
    And ruby gem for 'my_project' version '0.1.0' do exist in 'pkg'

  Scenario: generate native gem
    Given a safe project directory
    And a gem named 'my_project'
    And a extension named 'extension_one'
    And I've already successfully executed rake task 'compile'
    And 'pkg' folder is deleted
    When rake task 'native gem' is invoked
    Then rake task 'native gem' succeeded
    And 'pkg' folder is created
    And ruby gem for 'my_project' version '0.1.0' do exist in 'pkg'
    And binary gem for 'my_project' version '0.1.0' do exist in 'pkg'

  Scenario: generate forced native gem
    Given a safe project directory
    And a gem named 'my_project'
    And a extension 'extension_one' with forced platform 'universal-unknown'
    And I've already successfully executed rake task 'compile'
    And 'pkg' folder is deleted
    When rake task 'native:universal-unknown gem' is invoked
    Then rake task 'native:universal-unknown gem' succeeded
    And 'pkg' folder is created
    And ruby gem for 'my_project' version '0.1.0' do exist in 'pkg'
    And a gem for 'my_project' version '0.1.0' platform 'universal-unknown' do exist in 'pkg'
