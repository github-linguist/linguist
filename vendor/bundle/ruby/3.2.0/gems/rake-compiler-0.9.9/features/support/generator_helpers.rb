module GeneratorHelpers
  def generate_scaffold_structure
    # create folder structure
    FileUtils.mkdir_p "lib"
    FileUtils.mkdir_p "tasks"
    FileUtils.mkdir_p "tmp"

    # create Rakefile loader
    File.open("Rakefile", 'w') do |rakefile|
      rakefile.puts template_rakefile.strip
    end
  end

  def generate_gem_task(gem_name)
    # create generic gem task
    File.open("tasks/gem.rake", 'w') do |gem_rake|
      gem_rake.puts template_rake_gemspec(gem_name)
    end
  end

  def generate_extension_task_for(extension_name, platform = nil)
    # create folder structure
    FileUtils.mkdir_p "ext/#{extension_name}"

    return if File.exist?("tasks/#{extension_name}.rake")

    # Building a gem?
    if File.exist?("tasks/gem.rake") then
      File.open("tasks/gem.rake", 'a+') do |ext_in_gem|
        if platform
          ext_in_gem.puts template_rake_extension_with_platform(extension_name, platform)
        else
          ext_in_gem.puts template_rake_extension(extension_name, true)
        end
      end
    else
      # create specific extension rakefile
      File.open("tasks/#{extension_name}.rake", 'w') do |ext_rake|
        ext_rake.puts template_rake_extension(extension_name)
      end
    end
  end

  def generate_cross_compile_extension_task_for(extension_name)
    # create folder structure
    FileUtils.mkdir_p "ext/#{extension_name}"

    return if File.exist?("tasks/#{extension_name}.rake")

    # create specific extension rakefile
    # Building a gem?
    if File.exist?("tasks/gem.rake") then
      File.open("tasks/gem.rake", 'a+') do |ext_in_gem|
        ext_in_gem.puts template_rake_extension_cross_compile(extension_name, true)
      end
    else
      File.open("tasks/#{extension_name}.rake", 'w') do |ext_rake|
        ext_rake.puts template_rake_extension_cross_compile(extension_name)
      end
    end
  end

  def generate_java_compile_extension_task_for(extension_name)
    # create folder structure
    FileUtils.mkdir_p "ext/#{extension_name}"

    return if File.exist?("tasks/#{extension_name}.rake")

    # create specific extension rakefile
    # Building a gem?
    if File.exist?("tasks/gem.rake") then
      File.open("tasks/gem.rake", 'a+') do |ext_in_gem|
        ext_in_gem.puts template_rake_extension_java_compile(extension_name, true)
      end
    else
      File.open("tasks/#{extension_name}.rake", 'w') do |ext_rake|
        ext_rake.puts template_rake_extension_java_compile(extension_name)
      end
    end
  end

  def generate_multi_cross_compile_extension_task_for(extension_name)
    # create folder structure
    FileUtils.mkdir_p "ext/#{extension_name}"

    return if File.exist?("tasks/#{extension_name}.rake")

    # create specific extension rakefile
    # Building a gem?
    if File.exist?("tasks/gem.rake") then
      File.open("tasks/gem.rake", 'a+') do |ext_in_gem|
        ext_in_gem.puts template_rake_extension_multi_cross_compile(extension_name)
      end
    end
  end

  def generate_source_code_for(extension_name)
    # source C file
    File.open("ext/#{extension_name}/source.c", 'w') do |c|
      c.puts template_source_c(extension_name)
    end

    # header H file
    File.open("ext/#{extension_name}/source.h", 'w') do |h|
      h.puts template_source_h
    end

    # extconf.rb file
    File.open("ext/#{extension_name}/extconf.rb", 'w') do |ext|
      ext.puts template_extconf(extension_name)
    end
  end

  def generate_java_source_code_for(extension_name)
    # source .java file
    File.open("ext/#{extension_name}/#{camelize(extension_name)}Service.java", 'w') do |c|
      c.puts template_source_java(extension_name)
    end
  end

end

World(GeneratorHelpers)
