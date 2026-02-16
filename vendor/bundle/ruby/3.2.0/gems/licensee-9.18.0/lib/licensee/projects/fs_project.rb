# frozen_string_literal: true

# Filesystem-based project
#
# Analyze a folder on the filesystem for license information
#
# Project files for this project type will contain the following keys:
#  :name - the relative file name
#  :dir  - the directory path containing the file
module Licensee
  module Projects
    class FSProject < Licensee::Projects::Project
      def initialize(path, **args)
        if ::File.file?(path)
          @pattern = File.basename(path)
          @dir = File.expand_path File.dirname(path)
        else
          @pattern = '*'
          @dir = File.expand_path(path)
        end

        @root = File.expand_path(args.delete(:search_root) || @dir)
        raise 'Search root must be the project path directory or its ancestor' unless valid_search_root?

        super(**args)
      end

      private

      # Returns an array of hashes representing the project's files.
      # Hashes will have the the following keys:
      #  :name - the relative file name
      #  :dir  - the directory path containing the file
      def files
        @files ||= search_directories.flat_map do |dir|
          relative_dir = Pathname.new(dir).relative_path_from(dir_path).to_s
          Dir.glob(::File.join(dir, @pattern).tr('\\', '/')).filter_map do |file|
            next unless ::File.file?(file)

            { name: ::File.basename(file), dir: relative_dir }
          end
        end
      end

      # Retrieve a file's content from disk, enforcing encoding
      #
      # file - the file hash, with the :name key as the file's relative path
      #
      # Returns the file contents as a string
      def load_file(file)
        content = File.read dir_path.join(file[:dir], file[:name])
        content.force_encoding(ProjectFiles::ProjectFile::ENCODING)

        return content if content.valid_encoding?

        content.encode(ProjectFiles::ProjectFile::ENCODING, **ProjectFiles::ProjectFile::ENCODING_OPTIONS)
      end

      # Returns true if @dir is @root or it's descendant
      def valid_search_root?
        dir_path.fnmatch?(@root) || dir_path.fnmatch?(::File.join(@root, '**'))
      end

      # Returns the set of unique paths to search for project files
      # in order from @dir -> @root
      def search_directories
        search_enumerator.map(&:to_path)
                         .push(@root) # ensure root is included in the search
                         .uniq # don't include the root twice if @dir == @root
      end

      # Enumerates all directories to search, from @dir to @root
      def search_enumerator
        root = Pathname.new(@root)
        Enumerator.new do |yielder|
          dir_path.relative_path_from(root).ascend do |relative|
            yielder.yield root.join(relative)
          end
        end
      end

      def dir_path
        @dir_path ||= Pathname.new(@dir)
      end
    end
  end
end
