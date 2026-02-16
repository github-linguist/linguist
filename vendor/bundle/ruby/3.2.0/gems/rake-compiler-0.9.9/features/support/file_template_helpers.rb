module FileTemplateHelpers
  def template_rakefile
    <<-EOF
# add rake-compiler lib dir to the LOAD_PATH
$LOAD_PATH.unshift File.expand_path(File.join(File.dirname(__FILE__), '../..', 'lib'))

require 'rubygems'
require 'rake'

# load rakefile extensions (tasks)
Dir['tasks/*.rake'].each { |f| import f }
EOF
  end

  def template_rake_gemspec(gem_name)
    <<-EOF
require 'rubygems/package_task'
SPEC = Gem::Specification.new do |s|
  s.name = "#{gem_name}"
  s.version = "0.1.0"
  s.summary = "#{gem_name} test gem for rake-compiler"
  s.description = "#{gem_name} is a fake gem for testing under rake-compiler"

  s.files = FileList["ext/**/*.{rb,c,h}", "Rakefile", "tasks/*.rake", "lib/**/*.rb"]

  s.extensions = FileList["ext/**/extconf.rb"]

  s.homepage = 'http://github.com/luislavena/rake-compiler'
  s.rubyforge_project = 'TODO'

  s.authors = ["Luis Lavena"]
  s.email = ["luislavena@gmail.com"]
end

Gem::PackageTask.new(SPEC) do |pkg|
  pkg.need_zip = false
  pkg.need_tar = false
end
EOF
  end

  def template_rake_extension(extension_name, gem_spec = nil)
    <<-EOF
require 'rake/extensiontask'
Rake::ExtensionTask.new("#{extension_name}"#{', SPEC' if gem_spec})
EOF
  end

  def template_rake_extension_with_platform(extension_name, platform)
    <<-EOF
require 'rake/extensiontask'
Rake::ExtensionTask.new("#{extension_name}", SPEC) do |ext|
  ext.platform = "#{platform}"
end
EOF
  end

  def template_rake_extension_cross_compile(extension_name, gem_spec = nil)
    <<-EOF
require 'rake/extensiontask'
Rake::ExtensionTask.new("#{extension_name}"#{', SPEC' if gem_spec}) do |ext|
  ext.cross_compile = true
end
EOF
  end

  def template_rake_extension_multi_cross_compile(extension_name)
    <<-EOF
require 'rake/extensiontask'
Rake::ExtensionTask.new("#{extension_name}", SPEC) do |ext|
  ext.cross_compile = true
  ext.cross_platform = ['i386-mswin32-60', 'i386-mingw32']
end
EOF
  end

  def template_rake_extension_java_compile(extension_name, gem_spec = nil)
      <<-EOF
require 'rake/javaextensiontask'
Rake::JavaExtensionTask.new("#{extension_name}"#{', SPEC' if gem_spec}) do |ext|
  # nothing
end
EOF
  end

  def template_extconf(extension_name)
    <<-EOF
require 'mkmf'
create_makefile("#{extension_name}")
EOF
  end

  def template_source_c(extension_name)
    <<-EOF
#include "source.h"
void Init_#{extension_name}()
{
  printf("source.c of extension #{extension_name}\\n");
}
EOF
  end

  def template_source_h
    <<-EOF
#include "ruby.h"
EOF
  end

  def template_source_java(extension_name)
    <<-EOF
import org.jruby.Ruby;
import org.jruby.runtime.load.BasicLibraryService;

public class #{camelize(extension_name)}Service implements BasicLibraryService {
   public boolean basicLoad(final Ruby runtime) throws java.io.IOException {
     HelloWorldPrinter hwp = new HelloWorldPrinter();
     hwp.tellTheWorld();
     return true;
   }

   private class HelloWorldPrinter {
     void tellTheWorld() throws java.io.IOException {
       System.out.println("#{camelize(extension_name)}Service.java of extension #{extension_name}\\n");
     }
   }
}

EOF
  end

  def camelize(str)
    str.gsub(/(^|_)(.)/) { $2.upcase }
  end

end

World(FileTemplateHelpers)
