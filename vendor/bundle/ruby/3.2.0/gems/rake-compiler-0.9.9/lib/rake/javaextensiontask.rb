require 'rake/baseextensiontask'

# Define a series of tasks to aid in the compilation of Java extensions for
# gem developer/creators.

module Rake
  class JavaExtensionTask < BaseExtensionTask

    attr_accessor :classpath
    attr_accessor :debug

    # Provide source compatibility with specified release
    attr_accessor :source_version

    # Generate class files for specific VM version
    attr_accessor :target_version

    def platform
      @platform ||= 'java'
    end

    def java_compiling(&block)
      @java_compiling = block if block_given?
    end

    def init(name = nil, gem_spec = nil)
      super
      @source_pattern = '**/*.java'
      @classpath      = nil
      @java_compiling = nil
      @debug          = false
      @source_version = '1.5'
      @target_version = '1.5'
    end

    def define
      super

      define_java_platform_tasks
    end

    private
    def define_compile_tasks(for_platform = nil, ruby_ver = RUBY_VERSION)
      # platform usage
      platf = for_platform || platform

      # lib_path
      lib_path = lib_dir

      # tmp_path
      tmp_path = "#{@tmp_dir}/#{platf}/#{@name}"

      # cleanup and clobbering
      CLEAN.include(tmp_path)
      CLOBBER.include("#{lib_path}/#{binary(platf)}")
      CLOBBER.include("#{@tmp_dir}")

      # directories we need
      directory tmp_path
      directory lib_dir

      # copy binary from temporary location to final lib
      # tmp/extension_name/extension_name.{so,bundle} => lib/
      task "copy:#{@name}:#{platf}" => [lib_path, "#{tmp_path}/#{binary(platf)}"] do
        install "#{tmp_path}/#{binary(platf)}", "#{lib_path}/#{binary(platf)}"
      end

      file "#{tmp_path}/#{binary(platf)}" => "#{tmp_path}/.build" do

        class_files = FileList["#{tmp_path}/**/*.class"].
          gsub("#{tmp_path}/", '')

        # avoid environment variable expansion using backslash
        class_files.gsub!('$', '\$') unless windows?

        args = class_files.map { |path|
          ["-C #{tmp_path}", path]
        }.flatten

        sh "jar cf #{tmp_path}/#{binary(platf)} #{args.join(' ')}"
      end

      file "#{tmp_path}/.build" => [tmp_path] + source_files do
        not_jruby_compile_msg = <<-EOF
WARNING: You're cross-compiling a binary extension for JRuby, but are using
another interpreter. If your Java classpath or extension dir settings are not
correctly detected, then either check the appropriate environment variables or
execute the Rake compilation task using the JRuby interpreter.
(e.g. `jruby -S rake compile:java`)
        EOF
        warn_once(not_jruby_compile_msg) unless defined?(JRUBY_VERSION)

        classpath_arg = java_classpath_arg(@classpath)
        debug_arg     = @debug ? '-g' : ''

        sh "javac #{java_extdirs_arg} -target #{@target_version} -source #{@source_version} -Xlint:unchecked #{debug_arg} #{classpath_arg} -d #{tmp_path} #{source_files.join(' ')}"

        # Checkpoint file
        touch "#{tmp_path}/.build"
      end

      # compile tasks
      unless Rake::Task.task_defined?('compile') then
        desc "Compile all the extensions"
        task "compile"
      end

      # compile:name
      unless Rake::Task.task_defined?("compile:#{@name}") then
        desc "Compile #{@name}"
        task "compile:#{@name}"
      end

      # Allow segmented compilation by platform (open door for 'cross compile')
      task "compile:#{@name}:#{platf}" => ["copy:#{@name}:#{platf}"]
      task "compile:#{platf}" => ["compile:#{@name}:#{platf}"]

      # Only add this extension to the compile chain if current
      # platform matches the indicated one.
      if platf == RUBY_PLATFORM then
        # ensure file is always copied
        file "#{lib_path}/#{binary(platf)}" => ["copy:#{name}:#{platf}"]

        task "compile:#{@name}" => ["compile:#{@name}:#{platf}"]
        task "compile" => ["compile:#{platf}"]
      end
    end

    def define_java_platform_tasks
      # lib_path
      lib_path = lib_dir

      if @gem_spec && !Rake::Task.task_defined?("java:#{@gem_spec.name}")
        task "java:#{@gem_spec.name}" do |t|
          # FIXME: workaround Gem::Specification limitation around cache_file:
          # http://github.com/rubygems/rubygems/issues/78
          spec = gem_spec.dup
          spec.instance_variable_set(:"@cache_file", nil) if spec.respond_to?(:cache_file)

          # adjust to specified platform
          spec.platform = Gem::Platform.new('java')

          # clear the extensions defined in the specs
          spec.extensions.clear

          # add the binaries that this task depends on
          ext_files = []

          # go through native prerequisites and grab the real extension files from there
          t.prerequisites.each do |ext|
            ext_files << ext
          end

          # include the files in the gem specification
          spec.files += ext_files

          # expose gem specification for customization
          if @java_compiling
            @java_compiling.call(spec)
          end

          # Generate a package for this gem
          Gem::PackageTask.new(spec) do |pkg|
            pkg.need_zip = false
            pkg.need_tar = false
          end
        end

        # add binaries to the dependency chain
        task "java:#{@gem_spec.name}" => ["#{lib_path}/#{binary(platform)}"]

        # ensure the extension get copied
        unless Rake::Task.task_defined?("#{lib_path}/#{binary(platform)}") then
          file "#{lib_path}/#{binary(platform)}" => ["copy:#{name}:#{platform}"]
        end

        task 'java' => ["java:#{@gem_spec.name}"]
      end

      task 'java' do
        task 'compile' => 'compile:java'
      end
    end

    #
    # Discover Java Extension Directories and build an extdirs argument
    #
    def java_extdirs_arg
      extdirs = Java::java.lang.System.getProperty('java.ext.dirs') rescue nil
      extdirs = ENV['JAVA_EXT_DIR'] unless extdirs
      java_extdir = extdirs.nil? ? "" : "-extdirs \"#{extdirs}\""
    end

    #
    # Discover the Java/JRuby classpath and build a classpath argument
    #
    # @params
    #   *args:: Additional classpath arguments to append
    #
    # Copied verbatim from the ActiveRecord-JDBC project. There are a small myriad
    # of ways to discover the Java classpath correctly.
    #
    def java_classpath_arg(*args)
      jruby_cpath = nil
      if RUBY_PLATFORM =~ /java/
        begin
          cpath  = Java::java.lang.System.getProperty('java.class.path').split(File::PATH_SEPARATOR)
          cpath += Java::java.lang.System.getProperty('sun.boot.class.path').split(File::PATH_SEPARATOR)
          jruby_cpath = cpath.compact.join(File::PATH_SEPARATOR)
        rescue => e
        end
      end
      unless jruby_cpath
        jruby_cpath = ENV['JRUBY_PARENT_CLASSPATH'] || ENV['JRUBY_HOME'] &&
          Dir.glob("#{File.expand_path(ENV['JRUBY_HOME'])}/lib/*.jar").
            join(File::PATH_SEPARATOR)
      end
      raise "JRUBY_HOME or JRUBY_PARENT_CLASSPATH are not set" unless jruby_cpath
      jruby_cpath += File::PATH_SEPARATOR + args.join(File::PATH_SEPARATOR) unless args.empty?
      jruby_cpath ? "-cp \"#{jruby_cpath}\"" : ""
    end

  end
end
