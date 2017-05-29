$LOAD_PATH[0, 0] = File.join(File.dirname(__FILE__), '..', 'lib')

require 'linguist'

# strategies list
# STRATEGIES = [
#   Linguist::Strategy::Modeline,
#   Linguist::Strategy::Filename,
#   Linguist::Shebang,
#   Linguist::Strategy::Extension,
#   Linguist::Heuristics,
#   Linguist::Classifier
# ]

class StrategyInstrumenter
  # TODO: this is wrong in the docs: we should 1) pass an instance XOR 2) use a class method instead
  #
  # and it seems that there's a bug...
  # block is not properly yield when the custom instrumented is set
  # see: https://github.com/javierhonduco/linguist/commit/00a436f1757c8e0f13ee4de6fa390b13059a53fd
  def self.instrument(name, payload = {})
    if /detected/ =~ name
      warn ""
      warn "\e[32mwith strategy: #{payload[:strategy]}\e[0m"
      warn ""
    end
    yield if block_given?
    # the above is a temporary band-aid until a proper patch is
    # submitted:
    #diff --git a/lib/linguist.rb b/lib/linguist.rb
    #index c2065bf..4893c95 100644
    #--- a/lib/linguist.rb
    #+++ b/lib/linguist.rb
    #@@ -92,9 +92,8 @@ class << Linguist
    #   def instrument(*args, &bk)
    #          if instrumenter
    #                   instrumenter.instrument(*args, &bk)
    #                   -    elsif block_given?
    #                   -      yield
    #                        end
    #          +    yield if block_given?
    #             end
    #
    #    end
  end
end

Linguist.instrumenter = StrategyInstrumenter

def classify_file(path)
  blob = Linguist::FileBlob.new(path, Dir.pwd)
  type = if blob.text?
      'Text'
    elsif blob.image?
      'Image'
    else
      'Binary'
    end

  puts "#{blob.name}: #{blob.loc} lines (#{blob.sloc} sloc)"
  puts "  type:      #{type}"
  puts "  mime type: #{blob.mime_type}"
  puts "  language:  #{blob.language}"

  if blob.large?
    puts "  blob is too large to be shown"
  end

  if blob.generated?
    puts "  appears to be generated source code"
  end

  if blob.vendored?
    puts "  appears to be a vendored file"
  end
end

if __FILE__ == $0
  path = ARGV.first || __FILE__
  classify_file(path)
end
