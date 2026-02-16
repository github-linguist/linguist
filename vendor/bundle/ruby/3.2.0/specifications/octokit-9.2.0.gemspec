# -*- encoding: utf-8 -*-
# stub: octokit 9.2.0 ruby lib

Gem::Specification.new do |s|
  s.name = "octokit".freeze
  s.version = "9.2.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 1.3.5".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "rubygems_mfa_required" => "true" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["Wynn Netherland".freeze, "Erik Michaels-Ober".freeze, "Clint Shryock".freeze]
  s.date = "2024-10-16"
  s.description = "Simple wrapper for the GitHub API".freeze
  s.email = ["wynn.netherland@gmail.com".freeze, "sferik@gmail.com".freeze, "clint@ctshryock.com".freeze]
  s.homepage = "https://github.com/octokit/octokit.rb".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 2.7.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Ruby toolkit for working with the GitHub API".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_runtime_dependency(%q<faraday>.freeze, [">= 1", "< 3"])
  s.add_runtime_dependency(%q<sawyer>.freeze, ["~> 0.9"])
end
