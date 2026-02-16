# -*- encoding: utf-8 -*-
# stub: pathname-common_prefix 0.0.2 ruby lib

Gem::Specification.new do |s|
  s.name = "pathname-common_prefix".freeze
  s.version = "0.0.2"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.metadata = { "source_code_uri" => "https://gitlab.com/KitaitiMakoto/pathname-common_prefix" } if s.respond_to? :metadata=
  s.require_paths = ["lib".freeze]
  s.authors = ["KITAITI Makoto".freeze]
  s.date = "2023-12-24"
  s.description = "This file provides `Pathname.common_prefix` and `Pathname#common_prefix` which calculate the common prefix in the passed paths.".freeze
  s.email = ["KitaitiMakoto@gmail.com".freeze]
  s.executables = ["common-prefix".freeze]
  s.files = ["bin/common-prefix".freeze]
  s.homepage = "https://gitlab.com/KitaitiMakoto/pathname-common_prefix".freeze
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Calculate prefix commont to some pathnames".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version
end
