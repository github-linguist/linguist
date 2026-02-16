require 'pathname'

class Pathname
  class << self
    def common_prefix(*paths)
      paths.flatten!
      return if paths.empty?
      # @type var paths: NonEmptyArray[string | Pathname]
      Pathname(paths.pop).common_prefix(paths)
    end
  end

  def common_prefix(*others)
    others.flatten!
    # @type var others: Array[string | Pathname]
    others.map! {|path| Pathname path}
    # @type var others: Array[Pathname]
    enum_for(:ascend).find {|path|
      # @type path: pathnamable
      others.all? {|other|other.start_with?(path)}
    }
  end

  def start_with?(prefix)
    enum_for(:descend).include?(Pathname(prefix))
  end
end
