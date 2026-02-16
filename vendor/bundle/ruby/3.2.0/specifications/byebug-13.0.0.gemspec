# -*- encoding: utf-8 -*-
# stub: byebug 13.0.0 ruby lib
# stub: ext/byebug/extconf.rb

Gem::Specification.new do |s|
  s.name = "byebug".freeze
  s.version = "13.0.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["David Rodriguez".freeze, "Kent Sibilev".freeze, "Mark Moseley".freeze]
  s.bindir = "exe".freeze
  s.date = "2026-01-15"
  s.description = "Byebug is a Ruby debugger. It's implemented using the\n    TracePoint C API for execution control and the Debug Inspector C API for\n    call stack navigation.  The core component provides support that front-ends\n    can build on. It provides breakpoint handling and bindings for stack frames\n    among other things and it comes with an easy to use command line interface.".freeze
  s.email = "deivid.rodriguez@riseup.net".freeze
  s.executables = ["byebug".freeze]
  s.extensions = ["ext/byebug/extconf.rb".freeze]
  s.extra_rdoc_files = ["CHANGELOG.md".freeze, "CONTRIBUTING.md".freeze, "README.md".freeze, "GUIDE.md".freeze]
  s.files = ["CHANGELOG.md".freeze, "CONTRIBUTING.md".freeze, "GUIDE.md".freeze, "README.md".freeze, "exe/byebug".freeze, "ext/byebug/extconf.rb".freeze]
  s.homepage = "https://github.com/deivid-rodriguez/byebug".freeze
  s.licenses = ["BSD-2-Clause".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 3.2.0".freeze)
  s.rubygems_version = "3.4.20".freeze
  s.summary = "Ruby fast debugger - base + CLI".freeze

  s.installed_by_version = "3.4.20" if s.respond_to? :installed_by_version

  s.specification_version = 4

  s.add_runtime_dependency(%q<reline>.freeze, [">= 0.6.0"])
  s.add_development_dependency(%q<bundler>.freeze, ["~> 2.0"])
end
