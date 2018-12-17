require_relative "./helper"

class TestGenerated < Minitest::Test
  include Linguist

  class DataLoadedError < StandardError; end

  def generated_without_loading_data(blob)
    begin
      assert Generated.generated?(blob, lambda { raise DataLoadedError.new }), "#{blob} was not recognized as a generated file"
    rescue DataLoadedError
      assert false, "Data was loaded when calling generated? on #{blob}"
    end
  end

  def generated_loading_data(blob)
    assert_raises(DataLoadedError, "Data wasn't loaded when calling generated? on #{blob}") do
      Generated.generated?(blob, lambda { raise DataLoadedError.new })
    end
    assert Generated.generated?(blob, lambda { IO.read(blob) }), "#{blob} was not recognized as a generated file"
  end

  def generated_fixture_without_loading_data(name)
    generated_without_loading_data(File.join(fixtures_path, name))
  end

  def generated_fixture_loading_data(name)
    generated_loading_data(File.join(fixtures_path, name))
  end

  def generated_sample_without_loading_data(name)
    generated_without_loading_data(File.join(samples_path, name))
  end

  def generated_sample_loading_data(name)
    generated_loading_data(File.join(samples_path, name))
  end

  def test_check_generated
    # Xcode project files
    generated_sample_without_loading_data("Binary/MainMenu.nib")
    generated_sample_without_loading_data("Dummy/foo.xcworkspacedata")
    generated_sample_without_loading_data("Dummy/foo.xcuserstate")

    # Cocoapods
    generated_sample_without_loading_data("Pods/Pods.xcodeproj")
    generated_sample_without_loading_data("Pods/SwiftDependency/foo.swift")
    generated_sample_without_loading_data("Pods/ObjCDependency/foo.h")
    generated_sample_without_loading_data("Pods/ObjCDependency/foo.m")
    generated_sample_without_loading_data("Dummy/Pods/Pods.xcodeproj")
    generated_sample_without_loading_data("Dummy/Pods/SwiftDependency/foo.swift")
    generated_sample_without_loading_data("Dummy/Pods/ObjCDependency/foo.h")
    generated_sample_without_loading_data("Dummy/Pods/ObjCDependency/foo.m")

    # Carthage
    generated_sample_without_loading_data("Carthage/Build/.Dependency.version")
    generated_sample_without_loading_data("Carthage/Build/iOS/Dependency.framework")
    generated_sample_without_loading_data("Carthage/Build/Mac/Dependency.framework")
    generated_sample_without_loading_data("src/Carthage/Build/.Dependency.version")
    generated_sample_without_loading_data("src/Carthage/Build/iOS/Dependency.framework")
    generated_sample_without_loading_data("src/Carthage/Build/Mac/Dependency.framework")

    # Go-specific vendored paths
    generated_sample_without_loading_data("go/vendor/github.com/foo.go")
    generated_sample_without_loading_data("go/vendor/golang.org/src/foo.c")
    generated_sample_without_loading_data("go/vendor/gopkg.in/some/nested/path/foo.go")

    # .NET designer file
    generated_sample_without_loading_data("Dummy/foo.designer.cs")
    generated_sample_without_loading_data("Dummy/foo.Designer.cs")
    generated_sample_without_loading_data("Dummy/foo.designer.vb")
    generated_sample_without_loading_data("Dummy/foo.Designer.vb")

    # Composer generated composer.lock file
    generated_sample_without_loading_data("JSON/composer.lock")

    # Node modules
    generated_sample_without_loading_data("Dummy/node_modules/foo.js")

    # npm shrinkwrap file
    generated_sample_without_loading_data("Dummy/npm-shrinkwrap.json")
    generated_sample_without_loading_data("Dummy/package-lock.json")

    # Godep saved dependencies
    generated_sample_without_loading_data("Godeps/Godeps.json")
    generated_sample_without_loading_data("Godeps/_workspace/src/github.com/kr/s3/sign.go")

    # Go module metadata files
    generated_sample_without_loading_data("Dummy/go.sum")

    # Generated by Zephir
    generated_sample_without_loading_data("C/exception.zep.c")
    generated_sample_without_loading_data("C/exception.zep.h")
    generated_sample_without_loading_data("PHP/exception.zep.php")

    # Minified files
    generated_sample_loading_data("JavaScript/jquery-1.6.1.min.js")

    # JS files with source map reference
    generated_sample_loading_data("JavaScript/namespace.js")

    # Source Map
    generated_fixture_without_loading_data("Data/bootstrap.css.map")
    generated_fixture_loading_data("Data/sourcemap.v3.map")
    generated_fixture_loading_data("Data/sourcemap.v1.map")

    # Yarn locfile
    generated_fixture_loading_data("Data/yarn.lock")

    # Specflow
    generated_fixture_without_loading_data("Features/BindingCulture.feature.cs")

    # JFlex
    generated_sample_loading_data("Java/JFlexLexer.java")

    # GrammarKit
    generated_sample_loading_data("Java/GrammarKit.java")

    # roxygen2
    generated_sample_loading_data("R/import.Rd")

    # PostScript
    generated_sample_loading_data("PostScript/lambda.pfa")

    # Perl ppport.h
    generated_fixture_loading_data("Generated/ppport.h")

    # Graphql Relay
    generated_sample_without_loading_data("Javascript/__generated__/App_user.graphql.js")

    # Game Maker Studio 2
    generated_sample_loading_data("JSON/GMS2_Project.yyp")
    generated_sample_loading_data("JSON/2ea73365-b6f1-4bd1-a454-d57a67e50684.yy")
    generated_fixture_loading_data("Generated/options_main.inherited.yy")

  end
end
