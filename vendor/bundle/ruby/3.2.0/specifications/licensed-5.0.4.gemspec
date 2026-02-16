# -*- encoding: utf-8 -*-
# stub: licensed 5.0.4 ruby lib

Gem::Specification.new do |s|
  s.name = "licensed".freeze
  s.version = "5.0.4"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["GitHub".freeze]
  s.bindir = "exe".freeze
  s.date = "2025-05-06"
  s.description = "Licensed automates extracting and validating the licenses of dependencies.".freeze
  s.email = ["opensource+licensed@github.com".freeze]
  s.executables = ["licensed".freeze]
  s.files = ["exe/licensed".freeze]
  s.homepage = "https://github.com/github/licensed".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 3.1.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Extract and validate the licenses of dependencies.".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_runtime_dependency(%q<csv>.freeze, ["~> 3.3"])
  s.add_runtime_dependency(%q<licensee>.freeze, ["~> 9.16"])
  s.add_runtime_dependency(%q<thor>.freeze, ["~> 1.2"])
  s.add_runtime_dependency(%q<pathname-common_prefix>.freeze, ["~> 0.0.1"])
  s.add_runtime_dependency(%q<tomlrb>.freeze, ["~> 2.0"])
  s.add_runtime_dependency(%q<ruby-xxHash>.freeze, ["~> 0.4.0"])
  s.add_runtime_dependency(%q<parallel>.freeze, ["~> 1.22"])
  s.add_runtime_dependency(%q<reverse_markdown>.freeze, [">= 2.1", "< 4.0"])
  s.add_runtime_dependency(%q<json>.freeze, ["~> 2.6"])
  s.add_development_dependency(%q<rake>.freeze, ["~> 13.0"])
  s.add_development_dependency(%q<minitest>.freeze, ["~> 5.17"])
  s.add_development_dependency(%q<minitest-hooks>.freeze, ["~> 1.5"])
  s.add_development_dependency(%q<mocha>.freeze, ["~> 2.0"])
  s.add_development_dependency(%q<rubocop-github>.freeze, ["~> 0.20"])
  s.add_development_dependency(%q<byebug>.freeze, ["~> 12.0"])
end
