=begin
%%{
  machine simple_scanner;

  action Emit {
    emit data[(ts+8)..(te-7)].pack('c*')
  }

  foo = 'STARTFOO' any+ :>> 'ENDFOO';
  
  main := |*
    foo => Emit;
    any;
  *|;
}%%
=end


# Scans a file for "STARTFOO[...]ENDFOO" blocks and outputs their contents.
#
# ENV['CHUNK_SIZE'] determines how much of the file to read in at a time, allowing you to control memory usage.
#
# Uses ragel's scanner functionality even though it's not strictly necessary.
class SimpleScanner
  attr_reader :path

  def initialize(path)
    @path = path
    %% write data;
    # % (this fixes syntax highlighting)
  end

  def emit(foo)
    $stdout.puts foo
  end
  
  def perform
    # So that ragel doesn't try to get it from data.length
    pe = :ignored
    eof = :ignored

    %% write init;
    # % (this fixes syntax highlighting)

    leftover = []
    
    File.open(path) do |f|
      while chunk = f.read(ENV['CHUNK_SIZE'].to_i)
        data = leftover + chunk.unpack('c*')
        p ||= 0
        pe = data.length

        %% write exec;
        # % (this fixes syntax highlighting)
        if ts
          leftover = data[ts..pe]
          p = p - ts
          ts = 0
        else
          leftover = []
          p = 0
        end
      end
    end
  end
end

s = SimpleScanner.new ARGV[0]
s.perform