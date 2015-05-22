module Linguist
  class Generated
    # Public: Is the blob a generated file?
    #
    # name - String filename
    # data - String blob data. A block also maybe passed in for lazy
    #        loading. This behavior is deprecated and you should always
    #        pass in a String.
    #
    # Return true or false
    def self.generated?(name, data)
      new(name, data).generated?
    end

    # Internal: Initialize Generated instance
    #
    # name - String filename
    # data - String blob data
    def initialize(name, data)
      @name = name
      @extname = File.extname(name)
      @_data = data
    end

    attr_reader :name, :extname

    # Lazy load blob data if block was passed in.
    #
    # Awful, awful stuff happening here.
    #
    # Returns String data.
    def data
      @data ||= @_data.respond_to?(:call) ? @_data.call() : @_data
    end

    # Public: Get each line of data
    #
    # Returns an Array of lines
    def lines
      # TODO: data should be required to be a String, no nils
      @lines ||= data ? data.split("\n", -1) : []
    end

    # Internal: Is the blob a generated file?
    #
    # Generated source code is suppressed in diffs and is ignored by
    # language statistics.
    #
    # Please add additional test coverage to
    # `test/test_blob.rb#test_generated` if you make any changes.
    #
    # Return true or false
    def generated?
      xcode_file? ||
      generated_net_designer_file? ||
      generated_net_specflow_feature_file? ||
      composer_lock? ||
      node_modules? ||
      godeps? ||
      generated_by_zephir? ||
      minified_files? ||
      source_map? ||
      compiled_coffeescript? ||
      generated_parser? ||
      generated_net_docfile? ||
      generated_postscript? ||
      compiled_cython_file? ||
      generated_protocol_buffer_go? ||
      generated_protocol_buffer? ||
      generated_apache_thrift? ||
      generated_jni_header? ||
      generated_unity3d_meta? ||
      vcr_cassette?
    end

    # Internal: Is the blob an Xcode file?
    #
    # Generated if the file extension is an Xcode
    # file extension.
    #
    # Returns true of false.
    def xcode_file?
      ['.nib', '.xcworkspacedata', '.xcuserstate'].include?(extname)
    end

    # Internal: Is the blob minified files?
    #
    # Consider a file minified if the average line length is
    # greater then 110c.
    #
    # Currently, only JS and CSS files are detected by this method.
    #
    # Returns true or false.
    def minified_files?
      return unless ['.js', '.css'].include? extname
      if lines.any?
        (lines.inject(0) { |n, l| n += l.length } / lines.length) > 110
      else
        false
      end
    end

    # Internal: Is the blob a generated source map?
    #
    # Source Maps usually have .css.map or .js.map extensions. In case they
    # are not following the name convention, detect them based on the content.
    #
    # Returns true or false.
    def source_map?
      return false unless extname.downcase == '.map'

      name =~ /(\.css|\.js)\.map$/i ||                 # Name convention
      lines[0] =~ /^{"version":\d+,/ ||                # Revision 2 and later begin with the version number
      lines[0] =~ /^\/\*\* Begin line maps\. \*\*\/{/  # Revision 1 begins with a magic comment
    end

    # Internal: Is the blob of JS generated by CoffeeScript?
    #
    # CoffeeScript is meant to output JS that would be difficult to
    # tell if it was generated or not. Look for a number of patterns
    # output by the CS compiler.
    #
    # Return true or false
    def compiled_coffeescript?
      return false unless extname == '.js'

      # CoffeeScript generated by > 1.2 include a comment on the first line
      if lines[0] =~ /^\/\/ Generated by /
        return true
      end

      if lines[0] == '(function() {' &&     # First line is module closure opening
          lines[-2] == '}).call(this);' &&  # Second to last line closes module closure
          lines[-1] == ''                   # Last line is blank

        score = 0

        lines.each do |line|
          if line =~ /var /
            # Underscored temp vars are likely to be Coffee
            score += 1 * line.gsub(/(_fn|_i|_len|_ref|_results)/).count

            # bind and extend functions are very Coffee specific
            score += 3 * line.gsub(/(__bind|__extends|__hasProp|__indexOf|__slice)/).count
          end
        end

        # Require a score of 3. This is fairly arbitrary. Consider
        # tweaking later.
        score >= 3
      else
        false
      end
    end

    # Internal: Is this a generated documentation file for a .NET assembly?
    #
    # .NET developers often check in the XML Intellisense file along with an
    # assembly - however, these don't have a special extension, so we have to
    # dig into the contents to determine if it's a docfile. Luckily, these files
    # are extremely structured, so recognizing them is easy.
    #
    # Returns true or false
    def generated_net_docfile?
      return false unless extname.downcase == ".xml"
      return false unless lines.count > 3

      # .NET Docfiles always open with <doc> and their first tag is an
      # <assembly> tag
      return lines[1].include?("<doc>") &&
        lines[2].include?("<assembly>") &&
        lines[-2].include?("</doc>")
    end

    # Internal: Is this a codegen file for a .NET project?
    #
    # Visual Studio often uses code generation to generate partial classes, and
    # these files can be quite unwieldy. Let's hide them.
    #
    # Returns true or false
    def generated_net_designer_file?
      name.downcase =~ /\.designer\.cs$/
    end
    
    # Internal: Is this a codegen file for Specflow feature file?
    #
    # Visual Studio's SpecFlow extension generates *.feature.cs files
    # from *.feature files, they are not meant to be consumed by humans. 
    # Let's hide them.
    #
    # Returns true or false
    def generated_net_specflow_feature_file?
      name.downcase =~ /\.feature\.cs$/
    end

    # Internal: Is the blob of JS a parser generated by PEG.js?
    #
    # PEG.js-generated parsers are not meant to be consumed by humans.
    #
    # Return true or false
    def generated_parser?
      return false unless extname == '.js'

      # PEG.js-generated parsers include a comment near the top  of the file
      # that marks them as such.
      if lines[0..4].join('') =~ /^(?:[^\/]|\/[^\*])*\/\*(?:[^\*]|\*[^\/])*Generated by PEG.js/
        return true
      end

      false
    end

    # Internal: Is the blob of PostScript generated?
    #
    # PostScript files are often generated by other programs. If they tell us so,
    # we can detect them.
    #
    # Returns true or false.
    def generated_postscript?
      return false unless ['.ps', '.eps'].include? extname

      # We analyze the "%%Creator:" comment, which contains the author/generator
      # of the file. If there is one, it should be in one of the first few lines.
      creator = lines[0..9].find {|line| line =~ /^%%Creator: /}
      return false if creator.nil?

      # Most generators write their version number, while human authors' or companies'
      # names don't contain numbers. So look if the line contains digits. Also
      # look for some special cases without version numbers.
      return creator =~ /[0-9]/ ||
        creator.include?("mpage") ||
        creator.include?("draw") ||
        creator.include?("ImageMagick")
    end

    def generated_protocol_buffer_go?
      return false unless extname == '.go'
      return false unless lines.count > 1

      return lines[0].include?("Code generated by protoc-gen-go")
    end

    # Internal: Is the blob a C++, Java or Python source file generated by the
    # Protocol Buffer compiler?
    #
    # Returns true of false.
    def generated_protocol_buffer?
      return false unless ['.py', '.java', '.h', '.cc', '.cpp'].include?(extname)
      return false unless lines.count > 1

      return lines[0].include?("Generated by the protocol buffer compiler.  DO NOT EDIT!")
    end
    
    # Internal: Is the blob generated by Apache Thrift compiler?
    #
    # Returns true or false
    def generated_apache_thrift?
      return false unless ['.rb', '.py', '.go', '.js', '.m', '.java', '.h', '.cc', '.cpp'].include?(extname)
      return false unless lines.count > 1

      return lines[0].include?("Autogenerated by Thrift Compiler") || lines[1].include?("Autogenerated by Thrift Compiler")
    end

    # Internal: Is the blob a C/C++ header generated by the Java JNI tool javah?
    #
    # Returns true of false.
    def generated_jni_header?
      return false unless extname == '.h'
      return false unless lines.count > 2

      return lines[0].include?("/* DO NOT EDIT THIS FILE - it is machine generated */") &&
               lines[1].include?("#include <jni.h>")
    end

    # Internal: Is the blob part of node_modules/, which are not meant for humans in pull requests.
    #
    # Returns true or false.
    def node_modules?
      !!name.match(/node_modules\//)
    end

    # Internal: Is the blob part of Godeps/,
    # which are not meant for humans in pull requests.
    #
    # Returns true or false.
    def godeps?
      !!name.match(/Godeps\//)
    end

    # Internal: Is the blob a generated php composer lock file?
    #
    # Returns true or false.
    def composer_lock?
      !!name.match(/composer\.lock/)
    end

    # Internal: Is the blob a generated by Zephir
    #
    # Returns true or false.
    def generated_by_zephir?
      !!name.match(/.\.zep\.(?:c|h|php)$/)
    end

    # Is the blob a VCR Cassette file?
    #
    # Returns true or false
    def vcr_cassette?
      return false unless extname == '.yml'
      return false unless lines.count > 2
      # VCR Cassettes have "recorded_with: VCR" in the second last line.
      return lines[-2].include?("recorded_with: VCR")
    end

    # Internal: Is this a compiled C/C++ file from Cython?
    #
    # Cython-compiled C/C++ files typically contain:
    # /* Generated by Cython x.x.x on ... */
    # on the first line.
    #
    # Return true or false
    def compiled_cython_file?
      return false unless ['.c', '.cpp'].include? extname
      return false unless lines.count > 1
      return lines[0].include?("Generated by Cython")
    end

    # Internal: Is this a metadata file from Unity3D?
    #
    # Unity3D Meta files start with:
    #   fileFormatVersion: X
    #   guid: XXXXXXXXXXXXXXX
    #
    # Return true or false
    def generated_unity3d_meta?
      return false unless extname == '.meta'
      return false unless lines.count > 1
      return lines[0].include?("fileFormatVersion: ")
    end
  end
end
