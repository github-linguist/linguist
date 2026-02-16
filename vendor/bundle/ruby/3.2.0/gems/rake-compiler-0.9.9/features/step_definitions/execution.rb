# FIXME: Make the Transform work
#
# Transform /^| on JRuby$/ do |step_arg|
#  / on JRuby/.match(step_arg) != nil
# end

Given %r{^I've already successfully executed rake task '(.*)'(| on JRuby)$} do |task_name, on_jruby|
  rake_cmd = "rake #{task_name}"
  rake_cmd = 'jruby -S ' << rake_cmd if on_jruby == ' on JRuby'
  emptyness = `#{rake_cmd} 2>&1`
  unless $?.success?
    warn emptyness
    raise "rake failed with #{$?.exitstatus}"
  end
end

When /^rake task '(.*)' is invoked(| on JRuby)$/ do |task_name, on_jruby|
  @output ||= {}
  @result ||= {}
  rake_cmd = "rake #{task_name}"
  rake_cmd = 'jruby -S ' << rake_cmd if on_jruby == ' on JRuby'
  @output[task_name] = `#{rake_cmd} 2>&1`
  @result[task_name] = $?.success?
end

Then /^rake task '(.*)' succeeded$/ do |task_name|
  if @result.nil? || !@result.include?(task_name) then
    raise "The task #{task_name} should be invoked first."
  else
    @result[task_name].should be_true
  end
end

Then /^rake task '(.*)' should fail$/ do |task_name|
  if @result.nil? || !@result.include?(task_name) then
    raise "The task #{task_name} should be invoked first."
  else
    @result[task_name].should be_false
  end
end

Then /^output of rake task '(.*)' (contains|do not contain) \/(.*)\/$/ do |task_name, condition, regex|
  if condition == 'contains' then
    @output[task_name].should match(%r(#{regex}))
  else
    @output[task_name].should_not match(%r(#{regex}))
  end
end

Then /^output of rake task '(.*)' warns$/ do |task_name, warning|
  @output[task_name].should include(warning)
end
