module Linguist
  class Generated
    # Public: Is the blob a generated file?
    #
    # name - String filename
    # data - String blob data. A block also may be passed in for lazy
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
      cocoapods? ||
      carthage_build? ||
      generated_graphql_relay? ||
      generated_net_designer_file? ||
      generated_net_specflow_feature_file? ||
      composer_lock? ||
      cargo_lock? ||
      node_modules? ||
      go_vendor? ||
      go_lock? ||
      poetry_lock? ||
      esy_lock? ||
      npm_shrinkwrap_or_package_lock? ||
      generated_yarn_plugnplay? ||
      godeps? ||
      generated_by_zephir? ||
      minified_files? ||
      has_source_map? ||
      source_map? ||
      compiled_coffeescript? ||
      generated_parser? ||
      generated_net_docfile? ||
      generated_postscript? ||
      compiled_cython_file? ||
      pipenv_lock? ||
      generated_go? ||
      generated_protocol_buffer? ||
      generated_javascript_protocol_buffer? ||
      generated_apache_thrift? ||
      generated_jni_header? ||
      vcr_cassette? ||
      generated_antlr? ||
      generated_module? ||
      generated_unity3d_meta? ||
      generated_racc? ||
      generated_jflex? ||
      generated_grammarkit? ||
      generated_roxygen2? ||
      generated_html? ||
      generated_jison? ||
      generated_grpc_cpp? ||
      generated_dart? ||
      generated_perl_ppport_header? ||
      generated_gamemakerstudio? ||
      generated_gimp? ||
      generated_visualstudio6? ||
      generated_haxe? ||
      generated_jooq? ||
      generated_pascal_tlb?
    end

    # Internal: Is the blob an Xcode file?
    #
    # Generated if the file extension is an Xcode
    # file extension.
    #
    # Returns true or false.
    def xcode_file?
      ['.nib', '.xcworkspacedata', '.xcuserstate'].include?(extname)
    end

    # Internal: Is the blob part of Pods/, which contains dependencies not meant for humans in pull requests.
    #
    # Returns true or false.
    def cocoapods?
      !!name.match(/(^Pods|\/Pods)\//)
    end

    # Internal: Is the blob part of Carthage/Build/, which contains dependencies not meant for humans in pull requests.
    #
    # Returns true or false.
    def carthage_build?
      !!name.match(/(^|\/)Carthage\/Build\//)
    end

    # Internal: Does extname indicate a filetype which is commonly minified?
    #
    # Returns true or false.
    def maybe_minified?
      ['.js', '.css'].include? extname.downcase
    end

    # Internal: Is the blob a minified file?
    #
    # Consider a file minified if the average line length is
    # greater then 110c.
    #
    # Currently, only JS and CSS files are detected by this method.
    #
    # Returns true or false.
    def minified_files?
      if maybe_minified? and lines.any?
        (lines.inject(0) { |n, l| n += l.length } / lines.length) > 110
      else
        false
      end
    end

    # Internal: Does the blob contain a source-map reference?
    #
    # We assume that if one of the last 2 lines starts with a source-map
    # reference, then the current file was generated from other files.
    #
    # We use the last 2 lines because the last line might be empty.
    #
    # Returns true or false.
    def has_source_map?
      return false unless maybe_minified?
      lines.last(2).any? { |l| l.match(/^\/[*\/][\#@] source(?:Mapping)?URL|sourceURL=/) }
    end

    # Internal: Is the blob a generated source-map?
    #
    # Source-maps usually have .css.map or .js.map extensions. In case they
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
      !!name.match(/\.designer\.(cs|vb)$/i)
    end

    # Internal: Is this a codegen file for Specflow feature file?
    #
    # Visual Studio's SpecFlow extension generates *.feature.cs files
    # from *.feature files, they are not meant to be consumed by humans.
    # Let's hide them.
    #
    # Returns true or false
    def generated_net_specflow_feature_file?
      !!name.match(/\.feature\.cs$/i)
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
      return false unless ['.ps', '.eps', '.pfa'].include? extname

      # Type 1 and Type 42 fonts converted to PostScript are stored as hex-encoded byte streams; these
      # streams are always preceded the `eexec` operator (if Type 1), or the `/sfnts` key (if Type 42).
      return true if data =~ /(\n|\r\n|\r)\s*(?:currentfile eexec\s+|\/sfnts\s+\[\1<)\h{8,}\1/

      # We analyze the "%%Creator:" comment, which contains the author/generator
      # of the file. If there is one, it should be in one of the first few lines.
      creator = lines[0..9].find {|line| line =~ /^%%Creator: /}
      return false if creator.nil?

      # Most generators write their version number, while human authors' or companies'
      # names don't contain numbers. So look if the line contains digits. Also
      # look for some special cases without version numbers.
      return true if creator =~ /[0-9]|draw|mpage|ImageMagick|inkscape|MATLAB/ ||
        creator =~ /PCBNEW|pnmtops|\(Unknown\)|Serif Affinity|Filterimage -tops/

      # EAGLE doesn't include a version number when it generates PostScript.
      # However, it does prepend its name to the document's "%%Title" field.
      !!creator.include?("EAGLE") and lines[0..4].find {|line| line =~ /^%%Title: EAGLE Drawing /}
    end

    def generated_go?
      return false unless extname == '.go'
      return false unless lines.count > 1

      return lines.first(40).any? { |l| l.include? "Code generated by" }
    end

    PROTOBUF_EXTENSIONS = ['.py', '.java', '.h', '.cc', '.cpp', '.m', '.rb', '.php']

    # Internal: Is the blob a C++, Java or Python source file generated by the
    # Protocol Buffer compiler?
    #
    # Returns true or false.
    def generated_protocol_buffer?
      return false unless PROTOBUF_EXTENSIONS.include?(extname)
      return false unless lines.count > 1

      return lines.first(3).any? { |l| l.include?("Generated by the protocol buffer compiler.  DO NOT EDIT!") }
    end

    # Internal: Is the blob a Javascript source file generated by the
    # Protocol Buffer compiler?
    #
    # Returns true or false.
    def generated_javascript_protocol_buffer?
      return false unless extname == ".js"
      return false unless lines.count > 6

      return lines[5].include?("GENERATED CODE -- DO NOT EDIT!")
    end

    APACHE_THRIFT_EXTENSIONS = ['.rb', '.py', '.go', '.js', '.m', '.java', '.h', '.cc', '.cpp', '.php']

    # Internal: Is the blob generated by Apache Thrift compiler?
    #
    # Returns true or false
    def generated_apache_thrift?
      return false unless APACHE_THRIFT_EXTENSIONS.include?(extname)
      return lines.first(6).any? { |l| l.include?("Autogenerated by Thrift Compiler") }
    end

    # Internal: Is the blob a C/C++ header generated by the Java JNI tool javah?
    #
    # Returns true or false.
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

    # Internal: Is the blob part of the Go vendor/ tree,
    # not meant for humans in pull requests.
    #
    # Returns true or false.
    def go_vendor?
      !!name.match(/vendor\/((?!-)[-0-9A-Za-z]+(?<!-)\.)+(com|edu|gov|in|me|net|org|fm|io)/)
    end

    # Internal: Is the blob a generated Go dep or glide lock file?
    #
    # Returns true or false.
    def go_lock?
      !!name.match(/(Gopkg|glide)\.lock/)
    end

    # Internal: Is the blob a generated poetry.lock?
    #
    # Returns true or false.
    def poetry_lock?
      !!name.match(/poetry\.lock/)
    end

    # Internal: Is the blob a generated esy lock file?
    #
    # Returns true or false.
    def esy_lock?
      !!name.match(/(^|\/)(\w+\.)?esy.lock$/)
    end

    # Internal: Is the blob a generated npm shrinkwrap or package lock file?
    #
    # Returns true or false.
    def npm_shrinkwrap_or_package_lock?
      !!name.match(/npm-shrinkwrap\.json/) || !!name.match(/package-lock\.json/)
    end

    # Internal: Is the blob a generated Yarn Plug'n'Play?
    #
    # Returns true or false.
    def generated_yarn_plugnplay?
      !!name.match(/(^|\/)\.pnp\.(c|m)?js$/)
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

    # Internal: Is the blob generated by Zephir?
    #
    # Returns true or false.
    def generated_by_zephir?
      !!name.match(/.\.zep\.(?:c|h|php)$/)
    end

    # Internal: Is the blob a generated Rust Cargo lock file?
    #
    # Returns true or false.
    def cargo_lock?
      !!name.match(/Cargo\.lock/)
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

    # Is this a generated ANTLR file?
    #
    # Returns true or false
    def generated_antlr?
      return false unless extname == '.g'
      return false unless lines.count > 2
      return lines[1].include?("generated by Xtest")
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

    # Internal: Is this a Pipenv lock file?
    #
    # Returns true or false.
    def pipenv_lock?
      !!name.match(/Pipfile\.lock/)
    end

    # Internal: Is it a KiCAD or GFortran module file?
    #
    # KiCAD module files contain:
    # PCBNEW-LibModule-V1  yyyy-mm-dd h:mm:ss XM
    # on the first line.
    #
    # GFortran module files contain:
    # GFORTRAN module version 'x' created from
    # on the first line.
    #
    # Return true or false
    def generated_module?
      return false unless extname == '.mod'
      return false unless lines.count > 1
      return lines[0].include?("PCBNEW-LibModule-V") ||
              lines[0].include?("GFORTRAN module version '")
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

    # Internal: Is this a Racc-generated file?
    #
    # A Racc-generated file contains:
    # # This file is automatically generated by Racc x.y.z
    # on the third line.
    #
    # Return true or false
    def generated_racc?
      return false unless extname == '.rb'
      return false unless lines.count > 2
      return lines[2].start_with?("# This file is automatically generated by Racc")
    end

    # Internal: Is this a JFlex-generated file?
    #
    # A JFlex-generated file contains:
    # /* The following code was generated by JFlex x.y.z on d/at/e ti:me */
    # on the first line.
    #
    # Return true or false
    def generated_jflex?
      return false unless extname == '.java'
      return false unless lines.count > 1
      return lines[0].start_with?("/* The following code was generated by JFlex ")
    end

    # Internal: Is this a GrammarKit-generated file?
    #
    # A GrammarKit-generated file typically contain:
    # // This is a generated file. Not intended for manual editing.
    # on the first line. This is not always the case, as it's possible to
    # customize the class header.
    #
    # Return true or false
    def generated_grammarkit?
      return false unless extname == '.java'
      return false unless lines.count > 1
      return lines[0].start_with?("// This is a generated file. Not intended for manual editing.")
    end

    # Internal: Is this a roxygen2-generated file?
    #
    # A roxygen2-generated file typically contain:
    # % Generated by roxygen2: do not edit by hand
    # on the first line.
    #
    # Return true or false
    def generated_roxygen2?
      return false unless extname == '.Rd'
      return false unless lines.count > 1

      return lines[0].include?("% Generated by roxygen2: do not edit by hand")
    end

    # Internal: Is this a Jison-generated file?
    #
    # Jison-generated parsers typically contain:
    # /* parser generated by jison
    # on the first line.
    #
    # Jison-generated lexers typically contain:
    # /* generated by jison-lex
    # on the first line.
    #
    # Return true or false
    def generated_jison?
      return false unless extname == '.js'
      return false unless lines.count > 1
      return lines[0].start_with?("/* parser generated by jison ") ||
             lines[0].start_with?("/* generated by jison-lex ")
    end

    # Internal: Is this a protobuf/grpc-generated C++ file?
    #
    # A generated file contains:
    # // Generated by the gRPC C++ plugin.
    # on the first line.
    #
    # Return true or false
    def generated_grpc_cpp?
      return false unless %w{.cpp .hpp .h .cc}.include? extname
      return false unless lines.count > 1
      return lines[0].start_with?("// Generated by the gRPC")
    end

    # Internal: Is this a generated Dart file?
    #
    # A dart-lang/appengine generated file contains:
    # // Generated code. Do not modify.
    # on the first line.
    #
    # An owl generated file contains:
    # // GENERATED CODE - DO NOT MODIFY
    # on the first line.
    #
    # Return true or false
    def generated_dart?
      return false unless extname == '.dart'
      return false unless lines.count > 1
      return lines.first.downcase =~ /generated code\W{2,3}do not modify/
    end

    # Internal: Is the file a generated Perl/Pollution/Portability header file?
    #
    # Returns true or false.
    def generated_perl_ppport_header?
        return false unless name.match(/ppport\.h$/)
        return false unless lines.count > 10
        return lines[8].include?("Automatically created by Devel::PPPort")
    end

    # Internal: Is this a relay-compiler generated graphql file?
    #
    # Return true or false
    def generated_graphql_relay?
      !!name.match(/__generated__\//)
    end

    # Internal: Is this a generated Game Maker Studio (2) metadata file?
    #
    # All Game Maker Studio 2 generated files will be JSON, .yy or .yyp, and have
    # a part that looks like "modelName: GMname" on the 3rd line
    #
    # Return true or false
    def generated_gamemakerstudio?
      return false unless ['.yy', '.yyp'].include? extname
      return false unless lines.count > 3
      return lines[2].match(/\"modelName\"\:\s*\"GM/) ||
             lines[0] =~ /^\d\.\d\.\d.+\|\{/
    end

    # Internal: Is this a generated GIMP C image file?
    #
    # GIMP saves C sources with one of two comment forms:
    # * `/* GIMP RGB C-Source image dump (<filename>.c) */` (C source export)
    # * `/*  GIMP header image file format (RGB): <filename>.h  */` (Header export)
    #
    # Return true or false
    def generated_gimp?
      return false unless ['.c', '.h'].include? extname
      return false unless lines.count > 0
      return lines[0].match(/\/\* GIMP [a-zA-Z0-9\- ]+ C\-Source image dump \(.+?\.c\) \*\//) ||
             lines[0].match(/\/\*  GIMP header image file format \([a-zA-Z0-9\- ]+\)\: .+?\.h  \*\//)
    end

    # Internal: Is this a generated Microsoft Visual Studio 6.0 build file?
    #
    # Return true or false
    def generated_visualstudio6?
      return false unless extname.downcase == '.dsp'
      lines.first(3).any? { |l| l.include? '# Microsoft Developer Studio Generated Build File' }
    end

    HAXE_EXTENSIONS = ['.js', '.py', '.lua', '.cpp', '.h', '.java', '.cs', '.php']

    # Internal: Is this a generated Haxe-generated source file?
    #
    # Return true or false
    def generated_haxe?
      return false unless HAXE_EXTENSIONS.include?(extname)
      return lines.first(3).any? { |l| l.include?("Generated by Haxe") }
    end

    # Internal: Is this a generated HTML file?
    #
    # HTML documents generated by authoring tools often include a
    # a <meta> tag in the header of the form:
    #
    #    <meta name="generator" content="DocGen v5.0.1" />
    #
    # Return true or false
    def generated_html?
      return false unless ['.html', '.htm', '.xhtml'].include? extname.downcase
      return false unless lines.count > 1

      # Pkgdown
      return true if lines[0..1].any? do |line|
        line.match(/<!-- Generated by pkgdown: do not edit by hand -->/)
      end

      # Mandoc
      return true if lines.count > 2 && lines[2].start_with?('<!-- This is an automatically generated file.')

      # Doxygen
      return true if lines[0..30].any? do |line|
        line.match(/<!--\s+Generated by Doxygen\s+[.0-9]+\s*-->/i)
      end

      # HTML tag: <meta name="generator" content="…" />
      matches = lines[0..30].join(' ').scan(/<meta(\s+[^>]++)>/i)
      return false if matches.empty?
      return matches.map {|x| extract_html_meta(x) }.any? do |attr|
        attr["name"].to_s.downcase == 'generator' &&
        [attr["content"], attr["value"]].any? do |cv|
          !cv.nil? &&
          cv.match(/^
            ( org \s+ mode
            | j?latex2html
            | groff
            | makeinfo
            | texi2html
            | ronn
            ) \b
          /ix)
        end
      end
    end

    # Internal: Is this a generated jOOQ file?
    #
    # Return true or false
    def generated_jooq?
      return false unless extname.downcase == '.java'
      lines.first(2).any? { |l| l.include? 'This file is generated by jOOQ.' }
    end

    # Internal: Is this a generated Delphi Interface file for a type library?
    #
    # Delphi Type Library Import tool generates *_TLB.pas files based on .ridl files.
    # They are not meant to be altered by humans.
    #
    # Returns true or false
    def generated_pascal_tlb?
      !!name.match(/_tlb\.pas$/i)
    end

    # Internal: Extract a Hash of name/content pairs from an HTML <meta> tag
    def extract_html_meta(match)
      (match.last.sub(/\/\Z/, "").strip.scan(/
        (?<=^|\s)              # Check for preceding whitespace
        (name|content|value)   # Attribute names we're interested in
        \s* = \s*              # Key-value separator

        # Attribute value
        ( "[^"]+"        # name="value"
        | '[^']+'        # name='value'
        |  [^\s"']+      # name=value
        )
      /ix)).map do |match|
        key = match[0].downcase
        val = match[1].gsub(/\A["']|["']\Z/, '')
        [key, val]
      end.select { |x| x.length == 2 }.to_h
    end
  end
end
