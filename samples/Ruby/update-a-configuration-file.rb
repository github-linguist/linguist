require 'stringio'

class ConfigFile

  # create a ConfigFile object from a file
  def self.file(filename)
    fh = File.open(filename)
    obj = self.new(fh)
    obj.filename = filename
    fh.close
    obj
  end

  # create a ConfigFile object from a string
  def self.data(string)
    fh = StringIO.new(string)
    obj = self.new(fh)
    fh.close
    obj
  end

  def initialize(filehandle)
    @lines = filehandle.readlines
    @filename = nil
    tidy_file
  end
  attr :filename

  def save()
    if @filename
      File.open(@filename, "w") {|f| f.write(self)}
    end
  end

  def tidy_file()
    @lines.map! do |line|
      # remove leading whitespace
      line.lstrip!

      if line.match(/^#/)
        # Lines beginning with hash symbols should not be manipulated and left
        # unchanged in the revised file.
        line
      else
        # replace double semicolon prefixes with just a single semicolon
        line.sub!(/^;+\s+/, "; ")

        if line.match(/^; \s*$/)
          # Any lines beginning with a semicolon or groups of semicolons, but no
          # following option should be removed
          line = ""
        else
          # remove ... any trailing whitespace on the lines
          line = line.rstrip + "\n"

          # Whitespace between the option and paramters should consist only of a
          # single space
          if m = line.match(/^(; )?([[:upper:]]+)\s+(.*)/)
            line = (m[1].nil? ? "" : m[1]) + format_line(m[2], m[3])
          end
        end

        line
      end
    end
  end

  def format_line(option, value)
    "%s%s\n" % [option.upcase.strip, value.nil? ? "" : " " + value.to_s.strip]
  end

  # returns the index of the option, or nil if not found
  def find_option(option)
    @lines.find_index {|line| line.match(/^#{option.upcase.strip}\b/)}
  end

  # uncomments a disabled option
  def enable_option(option)
    if idx = find_option("; " + option)
      @lines[idx][/^; /] = ""
    end
  end

  # comment a line with a semi-colon
  def disable_option(option)
    if idx = find_option(option)
      @lines[idx][/^/] = "; "
    end
  end

  # add an option, or change the value of an existing option.
  # use nil for the value to set a boolean option
  def set_value(option, value)
    if idx = find_option(option)
      @lines[idx] = format_line(option, value)
    else
      @lines << format_line(option, value)
    end
  end

  def to_s
    @lines.join('')
  end
end


config = ConfigFile.data(DATA.read)
config.disable_option('needspeeling')
config.enable_option('seedsremoved')
config.set_value('numberofbananas', 1024)
config.set_value('numberofstrawberries', 62000)
puts config


__END__
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT 		banana

# This is a boolean that should be set
  NEEDSPEELING

# This boolean is commented out
;;; SEEDSREMOVED
;;;

# How many bananas we have
NUMBEROFBANANAS 48
