=begin
%%{
  machine simple_tokenizer;

  action MyTs {
    my_ts = p
  }
  action MyTe {
    my_te = p
  }
  action Emit {
    emit data[my_ts...my_te].pack('c*')
    my_ts = nil
    my_te = nil    
  }

  foo = 'STARTFOO' any+ >MyTs :>> 'ENDFOO' >MyTe %Emit;
  main := ( foo | any+ )*;

}%%
=end

# Scans a file for "STARTFOO[...]ENDFOO" blocks and outputs their contents.
#
# ENV['CHUNK_SIZE'] determines how much of the file to read in at a time, allowing you to control memory usage.
#
# Does not use ragel's scanner functionality because no backtracking is needed.
class SimpleTokenizer
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
    my_ts = nil
    my_te = nil
    
    File.open(path) do |f|
      while chunk = f.read(ENV['CHUNK_SIZE'].to_i)
        data = leftover + chunk.unpack('c*')
        p = 0
        pe = data.length
        %% write exec;
        # % (this fixes syntax highlighting)
        if my_ts
          leftover = data[my_ts..-1]
          my_te = my_te - my_ts if my_te
          my_ts = 0
        else
          leftover = []
        end
      end
    end
  end
end

s = SimpleTokenizer.new ARGV[0]
s.perform
