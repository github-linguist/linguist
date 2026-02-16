# -*- encoding: utf-8 -*-
# stub: mocha 2.8.2 ruby lib

Gem::Specification.new do |s|
  s.name = "mocha".freeze
  s.version = "2.8.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "bug_tracker_uri" => "https://github.com/freerange/mocha/issues", "changelog_uri" => "https://github.com/freerange/mocha/blob/main/RELEASE.md", "documentation_uri" => "https://mocha.jamesmead.org/", "funding_uri" => "https://github.com/sponsors/floehopper", "homepage_uri" => "https://mocha.jamesmead.org", "source_code_uri" => "https://github.com/freerange/mocha" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["James Mead".freeze]
  s.date = "2025-11-15"
  s.description = "Mocking and stubbing library with JMock/SchMock syntax, which allows mocking and stubbing of methods on real (non-mock) classes.".freeze
  s.email = "mocha-developer@googlegroups.com".freeze
  s.homepage = "https://mocha.jamesmead.org".freeze
  s.licenses = ["MIT".freeze, "BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.1".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Mocking and stubbing library".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_runtime_dependency(%q<ruby2_keywords>.freeze, [">= 0.0.5"])
end
