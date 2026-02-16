#--
# Cross-compile ruby, using Rake
#
# This source code is released under the MIT License.
# See LICENSE file for details
#++

#
# This code is inspired and based on notes from the following sites:
#
# http://tenderlovemaking.com/2008/11/21/cross-compiling-ruby-gems-for-win32/
# http://github.com/jbarnette/johnson/tree/master/cross-compile.txt
# http://eigenclass.org/hiki/cross+compiling+rcovrt
#
# This recipe only cleanup the dependency chain and automate it.
# Also opens the door to usage different ruby versions
# for cross-compilation.
#

require 'rake'
require 'rake/clean'

begin
  require 'psych'
rescue LoadError
end

require 'yaml'
require "rbconfig"

# load compiler helpers
# add lib directory to the search path
libdir = File.expand_path(File.join(File.dirname(__FILE__), '..', '..', 'lib'))
$LOAD_PATH.unshift(libdir) unless $LOAD_PATH.include?(libdir)

if RUBY_PLATFORM =~ /mingw|mswin/ then
  puts "This command is meant to be executed under Linux or OSX, not Windows (is for cross-compilation)"
  exit(1)
end

require 'rake/extensioncompiler'

MAKE = ENV['MAKE'] || %w[gmake make].find { |c| system("#{c} -v > /dev/null 2>&1") }
USER_HOME = File.expand_path("~/.rake-compiler")
RUBY_CC_VERSION = "ruby-" << ENV.fetch("VERSION", "1.8.7-p371")
RUBY_SOURCE = ENV['SOURCE']
RUBY_BUILD = RbConfig::CONFIG["host"]

# grab the major "1.8" or "1.9" part of the version number
MAJOR = RUBY_CC_VERSION.match(/.*-(\d.\d).\d/)[1]

# Use Rake::ExtensionCompiler helpers to find the proper host
MINGW_HOST = ENV['HOST'] || Rake::ExtensionCompiler.mingw_host
MINGW_TARGET = MINGW_HOST.gsub('msvc', '')

# Unset any possible variable that might affect compilation
["CC", "CXX", "CPPFLAGS", "LDFLAGS", "RUBYOPT"].each do |var|
  ENV.delete(var)
end

# define a location where sources will be stored
directory "#{USER_HOME}/sources/#{RUBY_CC_VERSION}"
directory "#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}"

# clean intermediate files and folders
CLEAN.include("#{USER_HOME}/sources/#{RUBY_CC_VERSION}")
CLEAN.include("#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}")

# remove the final products and sources
CLOBBER.include("#{USER_HOME}/sources")
CLOBBER.include("#{USER_HOME}/builds")
CLOBBER.include("#{USER_HOME}/ruby/#{MINGW_HOST}/#{RUBY_CC_VERSION}")
CLOBBER.include("#{USER_HOME}/config.yml")

# ruby source file should be stored there
file "#{USER_HOME}/sources/#{RUBY_CC_VERSION}.tar.bz2" => ["#{USER_HOME}/sources"] do |t|
  # download the source file using wget or curl
  chdir File.dirname(t.name) do
    if RUBY_SOURCE
      url = RUBY_SOURCE
    else
      url = "http://cache.ruby-lang.org/pub/ruby/#{MAJOR}/#{File.basename(t.name)}"
    end
    sh "wget #{url} || curl -O #{url}"
  end
end

# Extract the sources
source_file = RUBY_SOURCE ? RUBY_SOURCE.split('/').last : "#{RUBY_CC_VERSION}.tar.bz2"
file "#{USER_HOME}/sources/#{RUBY_CC_VERSION}" => ["#{USER_HOME}/sources/#{source_file}"] do |t|
  chdir File.dirname(t.name) do
    t.prerequisites.each { |f| sh "tar xf #{File.basename(f)}" }
  end
end

# backup makefile.in
file "#{USER_HOME}/sources/#{RUBY_CC_VERSION}/Makefile.in.bak" => ["#{USER_HOME}/sources/#{RUBY_CC_VERSION}"] do |t|
  cp "#{USER_HOME}/sources/#{RUBY_CC_VERSION}/Makefile.in", t.name
end

# correct the makefiles
file "#{USER_HOME}/sources/#{RUBY_CC_VERSION}/Makefile.in" => ["#{USER_HOME}/sources/#{RUBY_CC_VERSION}/Makefile.in.bak"] do |t|
  content = File.open(t.name, 'rb') { |f| f.read }

  out = ""

  content.each_line do |line|
    if line =~ /^\s*ALT_SEPARATOR =/
      out << "\t\t    ALT_SEPARATOR = \"\\\\\\\\\"; \\\n"
    else
      out << line
    end
  end

  when_writing("Patching Makefile.in") {
    File.open(t.name, 'wb') { |f| f.write(out) }
  }
end

task :mingw32 do
  unless MINGW_HOST then
    warn "You need to install mingw32 cross compile functionality to be able to continue."
    warn "Please refer to your distribution/package manager documentation about installation."
    fail
  end
end

# generate the makefile in a clean build location
file "#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}/Makefile" => ["#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}",
                                  "#{USER_HOME}/sources/#{RUBY_CC_VERSION}/Makefile.in"] do |t|

  options = [
    "--host=#{MINGW_HOST}",
    "--target=#{MINGW_TARGET}",
    "--build=#{RUBY_BUILD}",
    '--enable-shared',
    '--disable-install-doc',
    '--with-ext='
  ]

  # Force Winsock2 for Ruby 1.8, 1.9 defaults to it
  options << "--with-winsock2" if MAJOR == "1.8"

  chdir File.dirname(t.name) do
    prefix = File.expand_path("../../../ruby/#{MINGW_HOST}/#{RUBY_CC_VERSION}")
    options << "--prefix=#{prefix}"
    sh File.expand_path("../../../sources/#{RUBY_CC_VERSION}/configure"), *options
  end
end

# make
file "#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}/ruby.exe" => ["#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}/Makefile"] do |t|
  chdir File.dirname(t.prerequisites.first) do
    sh MAKE
  end
end

# make install
file "#{USER_HOME}/ruby/#{MINGW_HOST}/#{RUBY_CC_VERSION}/bin/ruby.exe" => ["#{USER_HOME}/builds/#{MINGW_HOST}/#{RUBY_CC_VERSION}/ruby.exe"] do |t|
  chdir File.dirname(t.prerequisites.first) do
    sh "#{MAKE} install"
  end
end
task :install => ["#{USER_HOME}/ruby/#{MINGW_HOST}/#{RUBY_CC_VERSION}/bin/ruby.exe"]

desc "Update rake-compiler list of installed Ruby versions"
task 'update-config' do
  config_file = "#{USER_HOME}/config.yml"
  if File.exist?(config_file) then
    puts "Updating #{config_file}"
    config = YAML.load_file(config_file)
  else
    puts "Generating #{config_file}"
    config = {}
  end

  files = Dir.glob("#{USER_HOME}/ruby/*/*/**/rbconfig.rb").sort

  files.each do |rbconfig|
    version, platform = rbconfig.match(/.*-(\d.\d.\d).*\/([-\w]+)\/rbconfig/)[1,2]
    platforms = [platform]

    # fake alternate (binary compatible) i386-mswin32-60 platform
    platform == "i386-mingw32" and
      platforms.push "i386-mswin32-60"

    platforms.each do |plat|
      config["rbconfig-#{plat}-#{version}"] = rbconfig

      # also store RubyGems-compatible version
      gem_platform = Gem::Platform.new(plat)
      config["rbconfig-#{gem_platform}-#{version}"] = rbconfig
    end

    puts "Found Ruby version #{version} for platform #{platform} (#{rbconfig})"
  end

  when_writing("Saving changes into #{config_file}") {
    File.open(config_file, 'w') do |f|
      f.puts config.to_yaml
    end
  }
end

task :default do
  # Force the display of the available tasks when no option is given
  Rake.application.options.show_task_pattern = //
  Rake.application.display_tasks_and_comments
end

desc "Build #{RUBY_CC_VERSION} suitable for cross-platform development."
task 'cross-ruby' => [:mingw32, :install, 'update-config']

