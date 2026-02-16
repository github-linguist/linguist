Given /^a safe project directory$/ do
  # step back to ROOT
  Dir.chdir ROOT_PATH
  tmp_name = "project.#{Process.pid}"
  @safe_dir = File.join(ROOT_PATH, 'tmp', tmp_name)
  FileUtils.rm_rf @safe_dir
  FileUtils.mkdir_p @safe_dir
  Dir.chdir @safe_dir

  generate_scaffold_structure
end

Given /^'(.*)' folder (exist|is deleted)$/ do |folder, condition|
  case condition
    when 'exist'
      raise "Folder #{folder} do not exist" unless File.exist?(folder) && File.directory?(folder)
    when 'is deleted'
      FileUtils.rm_rf folder
  end
end

Then /^'(.*)' folder is created$/ do |folder|
  File.directory?(folder).should be_true
end

Then /^'(.*)' folder do not exist$/ do |folder|
  File.directory?(folder).should_not be_true
end

Then /^no left over from '(.*)' remains in '(.*)'$/ do |name, folder|
  Dir.glob("#{folder}/**/#{name}/#{RUBY_VERSION}").should be_empty
end
