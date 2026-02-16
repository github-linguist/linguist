module Rake

  #
  # HACK: Lousy API design, sue me. At least works ;-)
  #
  # Define a series of helpers to aid in search and usage of MinGW (GCC) Compiler
  # by gem developer/creators.
  #
  module ExtensionCompiler
    # return the host portion from the installed MinGW
    def self.mingw_host
      return @mingw_host if @mingw_host

      # the mingw_gcc_executable is helpful here
      if target = mingw_gcc_executable then
        # we only care for the filename
        target = File.basename(target)

        # now strip the extension (if present)
        target.sub!(File.extname(target), '')

        # get rid of '-gcc' portion too ;-)
        target.sub!('-gcc', '')
      end

      raise "No MinGW tools or unknown setup platform?" unless target

      @mingw_host = target
    end

    # return the first compiler found that includes both mingw and gcc conditions
    # (this assumes you have one installed)
    def self.mingw_gcc_executable
      return @mingw_gcc_executable if @mingw_gcc_executable

      # grab the paths defined in the environment
      paths = ENV['PATH'].split(File::PATH_SEPARATOR)

      # the pattern to look into (captures *nix and windows executables)
      pattern = "{mingw32-,i?86*mingw*}gcc{,.*}"

      @mingw_gcc_executable = paths.find do |path|
        # cleanup paths before globbing
        gcc = Dir.glob("#{File.expand_path(path)}/#{pattern}").first
        break gcc if gcc
      end

      @mingw_gcc_executable
    end
  end
end
