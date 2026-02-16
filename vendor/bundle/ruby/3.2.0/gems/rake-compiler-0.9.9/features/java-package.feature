Feature: Generate JRuby gems from JRuby or MRI

  In order to keep sanity in the Ruby world
  As a Gem developer who used to do J2EE development
  I want more rake magic that turns monotony into joy.

  @java
  Scenario: package a gem for Java (with default Rake)
    Given that my JRuby gem source is all in place
    And I've installed the Java Development Kit
    And I've already successfully executed rake task 'java compile'
    When rake task 'java gem' is invoked
    Then rake task 'java gem' succeeded
    And gem for platform 'java' get generated

  @java
  Scenario: package a gem for Java (with Rake on JRuby)
    Given that my JRuby gem source is all in place
    And I've installed the Java Development Kit
    And I've installed JRuby
    And I've already successfully executed rake task 'java compile' on JRuby
    When rake task 'java gem' is invoked
    Then rake task 'java gem' succeeded
    And gem for platform 'java' get generated
