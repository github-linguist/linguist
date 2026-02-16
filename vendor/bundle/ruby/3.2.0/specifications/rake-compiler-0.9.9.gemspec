# -*- encoding: utf-8 -*-
# stub: rake-compiler 0.9.9 ruby lib

Gem::Specification.new do |s|
  s.name = "rake-compiler".freeze
  s.version = "0.9.9"

  s.required_rubygems_version = Gem::Requirement.new(">= 1.8.23".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Kouhei Sutou".freeze, "Luis Lavena".freeze]
  s.date = "2016-05-10"
  s.description = "Provide a standard and simplified way to build and package\nRuby extensions (C, Java) using Rake as glue.".freeze
  s.email = ["kou@cozmixng.org".freeze, "luislavena@gmail.com".freeze]
  s.executables = ["rake-compiler".freeze]
  s.extra_rdoc_files = ["README.rdoc".freeze, "LICENSE.txt".freeze, "History.txt".freeze]
  s.files = ["History.txt".freeze, "LICENSE.txt".freeze, "README.rdoc".freeze, "bin/rake-compiler".freeze]
  s.homepage = "https://github.com/rake-compiler/rake-compiler".freeze
  s.licenses = ["MIT".freeze]
  s.rdoc_options = ["--main".freeze, "README.rdoc".freeze, "--title".freeze, "rake-compiler -- Documentation".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 1.8.7".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Rake-based Ruby Extension (C, Java) task generator.".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_runtime_dependency(%q<rake>.freeze, [">= 0"])
  s.add_development_dependency(%q<rspec>.freeze, ["~> 2.8.0"])
  s.add_development_dependency(%q<cucumber>.freeze, ["~> 1.1.4"])
end
