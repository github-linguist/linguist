module PlatformExtensionHelpers
  def binary_extension(platform = RUBY_PLATFORM)
    case platform
      when /darwin/
        'bundle'
      when /mingw|mswin|linux/
        'so'
      when /java/
        'jar'
      else
        RbConfig::CONFIG['DLEXT']
    end
  end

  def search_path(binaries)
    paths = ENV['PATH'].split(File::PATH_SEPARATOR)
    binary = binaries.find do |bin_file|
      paths.find do |path|
        bin = File.join(path, bin_file)
        File.exist?(bin) && File.executable?(bin)
      end
    end
    binary
  end
end

World(PlatformExtensionHelpers)
