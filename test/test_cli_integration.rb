require_relative "./helper"
require 'tmpdir'
require 'fileutils'
require 'open3'

class TestCLIIntegration < Minitest::Test
  def setup
    @temp_dir = Dir.mktmpdir('linguist_cli_test')
    @original_dir = Dir.pwd
    Dir.chdir(@temp_dir)

    # Initialize a git repository
    system("git init --quiet")
    system("git config user.name 'Test User'")
    system("git config user.email 'test@example.com'")
  end

  def teardown
    Dir.chdir(@original_dir)
    FileUtils.rm_rf(@temp_dir)
  end

  def test_strategies_flag_with_gitattributes_override
    # Create a .gitattributes file that overrides language detection
    File.write('.gitattributes', "*.special linguist-language=Ruby\n")

    # Create a test file with a non-Ruby extension but Ruby content
    File.write('test.special', "puts 'Hello, World!'\n")

    # Stage and commit the files
    system("git add .")
    system("git commit -m 'Initial commit' --quiet")

    # Run github-linguist with --strategies flag from the original directory but pointing to our test file
    stdout, stderr, status = Open3.capture3(
      "bundle", "exec", "github-linguist", File.join(@temp_dir, "test.special"), "--strategies",
      chdir: @original_dir
    )

    assert status.success?, "CLI command failed: #{stderr}"
    assert_match(/language:\s+Ruby/, stdout, "Should detect Ruby language")
    assert_match(/strategy:\s+GitAttributes/, stdout, "Should show GitAttributes strategy")
  end

  def test_strategies_flag_with_normal_detection
    # Create a normal Ruby file
    File.write('test.rb', "puts 'Hello, World!'\n")

    # Stage and commit the file
    system("git add .")
    system("git commit -m 'Initial commit' --quiet")

    # Run github-linguist with --strategies flag
    stdout, stderr, status = Open3.capture3(
      "bundle", "exec", "github-linguist", File.join(@temp_dir, "test.rb"), "--strategies",
      chdir: @original_dir
    )

    assert status.success?, "CLI command failed: #{stderr}"
    assert_match(/language:\s+Ruby/, stdout, "Should detect Ruby language")
    assert_match(/strategy:\s+Extension/, stdout, "Should show Extension strategy")
  end

  def test_breakdown_with_gitattributes_strategies
    # Create multiple files with different detection methods
    File.write('.gitattributes', "*.special linguist-language=JavaScript\n")
    File.write('override.special', "console.log('overridden');\n")
    File.write('normal.js', "console.log('normal');\n")
    File.write('Dockerfile', "FROM ubuntu\n")

    # Stage and commit the files
    system("git add .")
    system("git commit -m 'Initial commit' --quiet")

    # Run github-linguist with --breakdown --strategies flags on the test repository
    stdout, stderr, status = Open3.capture3(
      "bundle", "exec", "github-linguist", @temp_dir, "--breakdown", "--strategies",
      chdir: @original_dir
    )

    assert status.success?, "CLI command failed: #{stderr}"

    # Check that GitAttributes strategy appears for the overridden file
    assert_match(/override\.special \[GitAttributes\]/, stdout, "Should show GitAttributes strategy for overridden file")

    # Check that normal detection strategies appear for other files
    assert_match(/normal\.js \[Extension\]/, stdout, "Should show Extension strategy for .js file")
    assert_match(/Dockerfile \[Filename\]/, stdout, "Should show Filename strategy for Dockerfile")
  end

  def test_json_output_preserves_functionality
    # Create a simple test file
    File.write('test.rb', "puts 'Hello, World!'\n")

    # Stage and commit the file
    system("git add .")
    system("git commit -m 'Initial commit' --quiet")

    # Run github-linguist with --json flag
    stdout, stderr, status = Open3.capture3(
      "bundle", "exec", "github-linguist", File.join(@temp_dir, "test.rb"), "--json",
      chdir: @original_dir
    )

    assert status.success?, "CLI command failed: #{stderr}"

    # Parse JSON output
    require 'json'
    result = JSON.parse(stdout)

    test_file_key = File.join(@temp_dir, "test.rb")
    assert_equal "Ruby", result[test_file_key]["language"], "JSON output should contain correct language"
    assert_equal "Text", result[test_file_key]["type"], "JSON output should contain correct type"
  end

  def test_repository_scan_with_gitattributes
    # Create a more complex repository structure
    FileUtils.mkdir_p('src')
    File.write('.gitattributes', "*.config linguist-language=JavaScript\n")
    File.write('src/app.rb', "class App\nend\n")
    File.write('config.config', "var x = 1;\n")

    # Stage and commit the files
    system("git add .")
    system("git commit -m 'Initial commit' --quiet")

    # Run github-linguist on the test repository
    stdout, stderr, status = Open3.capture3(
      "bundle", "exec", "github-linguist", @temp_dir, "--breakdown", "--strategies",
      chdir: @original_dir
    )

    assert status.success?, "CLI command failed: #{stderr}"

    # Verify that both normal and override detection work in repository scan
    assert_match(/src\/app\.rb \[Extension\]/, stdout, "Should show Extension strategy for Ruby file")
    assert_match(/config\.config \[GitAttributes\]/, stdout, "Should show GitAttributes strategy for overridden file")
  end
end
