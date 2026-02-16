require 'mkmf'

if `which make`.strip.empty?
  STDERR.puts "\n\n"
  STDERR.puts "***************************************************************************************"
  STDERR.puts "*************** make required (apt-get install make build-essential) =( ***************"
  STDERR.puts "***************************************************************************************"
  exit(1)
end

##
# ICU dependency
#

ldflags = cppflags = nil

if RbConfig::CONFIG["host_os"] =~ /darwin/
  begin
    brew_prefix = `brew --prefix icu4c`.chomp
    ldflags   = "#{brew_prefix}/lib"
    cppflags  = "#{brew_prefix}/include"
    pkg_conf  = "#{brew_prefix}/lib/pkgconfig"
    # pkg_config should be less error prone than parsing compiler
    # commandline options, but we need to set default ldflags and cpp flags
    # in case the user doesn't have pkg-config installed
    ENV['PKG_CONFIG_PATH'] ||= pkg_conf
  rescue
  end
end

dir_config 'icu', cppflags, ldflags

pkg_config("icu-i18n")
pkg_config("icu-io")
pkg_config("icu-uc")

unless have_library 'icui18n' and have_header 'unicode/ucnv.h'
  STDERR.puts "\n\n"
  STDERR.puts "***************************************************************************************"
  STDERR.puts "*********** icu required (brew install icu4c or apt-get install libicu-dev) ***********"
  STDERR.puts "***************************************************************************************"
  exit(1)
end

have_library 'z' or abort 'libz missing'
have_library 'icuuc' or abort 'libicuuc missing'
have_library 'icudata' or abort 'libicudata missing'

$CFLAGS << ' -Wall -funroll-loops'
$CFLAGS << ' -Wextra -O0 -ggdb3' if ENV['DEBUG']

minimal_program = <<~SRC
  #include <unicode/translit.h>
  int main() { return 0; }
SRC

# Pass -x c++ to force gcc to compile the test program
# as C++ (as it will end in .c by default).
compile_options = +"-x c++"

icu_requires_version_flag = checking_for("icu that requires explicit C++ version flag") do
  !try_compile(minimal_program, compile_options)
end

if icu_requires_version_flag
  abort "Cannot compile icu with your compiler: recent versions require C++17 support." unless %w[c++20 c++17 c++11 c++0x].any? do |std|
    checking_for("icu that compiles with #{std} standard") do
      flags = compile_options + " -std=#{std}"
      if try_compile(minimal_program, flags)
        $CPPFLAGS << flags

        true
      end
    end
  end
end

def libflag_to_filename(ldflag)
  case ldflag
  when /\A-l(.+)/
    "lib#{Regexp.last_match(1)}.#{$LIBEXT}"
  end
end

def resolve_static_library(libflag, dirs)
  filename = libflag_to_filename(libflag)

  dir = dirs.find { |path| File.exist?(File.join(path, filename)) }

  raise "Unable to find #{filename} in #{dirs}" unless dir

  File.join(dir, filename)
end

def substitute_static_libs(packages)
  packages.each do |pkg|
    unless pkg_config(pkg)
      message = <<~MSG
        Unable to run `pkg-config #{pkg}`.

        Check that PKG_CONFIG_PATH includes #{pkg}.pc (or unset it if it's already set).

        Current environment:
        PKG_CONFIG_PATH=#{ENV['PKG_CONFIG_PATH']}
      MSG

      raise message
    end
  end

  # First, find all the -l<lib> flags added by pkg-config. We want to drop
  # these dynamically linked libraries and substitute them with the static libraries.
  libflags = packages.map do |pkg|
    pkg_config(pkg, 'libs-only-l')&.strip&.split(' ')
  end.flatten.uniq

  # To find where the static libraries live, we need to search the
  # library paths given by the -L flag from pkg-config.
  lib_paths = packages.map do |pkg|
    include_path = pkg_config(pkg, 'libs-only-L')&.strip
    include_path&.split(' ')&.map { |lib| lib.gsub(/^-L/, '') }
  end.flatten.uniq

  # Drop the -l<lib> flags and add in the static libraries.
  new_libs = $libs.shellsplit
  new_libs.reject! { |arg| libflags.include?(arg) }
  libflags.each { |flag| new_libs << resolve_static_library(flag, lib_paths) }
  $libs = new_libs.uniq.shelljoin
end

static_p = enable_config('static', false)
message "Static linking is #{static_p ? 'enabled' : 'disabled'}.\n"

if static_p
  $CXXFLAGS << ' -fPIC'
  ENV['PKG_CONFIG_ALLOW_SYSTEM_LIBS'] = '1'

  substitute_static_libs(%w[icu-i18n icu-io icu-uc])
end

create_makefile 'charlock_holmes/charlock_holmes'
