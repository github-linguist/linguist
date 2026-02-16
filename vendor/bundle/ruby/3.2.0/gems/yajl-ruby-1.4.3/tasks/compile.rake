require 'rake/extensiontask'

def gemspec
  @clean_gemspec ||= eval(File.read(File.expand_path('../../yajl-ruby.gemspec', __FILE__)))
end

Rake::ExtensionTask.new('yajl', gemspec) do |ext|
  # automatically add build options to avoid need of manual input
  ext.cross_compile = true
  ext.cross_platform = ['x86-mingw32', 'x86-mswin32-60']

  # inject 1.8/1.9 pure-ruby entry point when cross compiling only
  ext.cross_compiling do |spec|
    spec.files << 'lib/yajl/yajl.rb'
  end

  ext.lib_dir = File.join 'lib', 'yajl'

  # clean compiled extension
  CLEAN.include "#{ext.lib_dir}/*.#{RbConfig::CONFIG['DLEXT']}"
end
Rake::Task[:spec].prerequisites << :compile

file 'lib/yajl/yajl.rb' do |t|
  File.open(t.name, 'wb') do |f|
    f.write <<-eoruby
RUBY_VERSION =~ /(\\d+.\\d+)/
require "yajl/\#{$1}/yajl"
    eoruby
  end
end

if Rake::Task.task_defined?(:cross)
  Rake::Task[:cross].prerequisites << 'lib/yajl/yajl.rb'
end
