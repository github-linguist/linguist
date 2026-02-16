Given %r{^I've installed the Java Development Kit$} do
  pending('Cannot locate suitable Java compiler (the Java Development Kit) in the PATH.') unless search_path(%w(javac javac.exe))
end

Given %r{^I've installed JRuby$} do
  pending('Cannot locate a JRuby installation in the PATH.') unless search_path(%w(jruby jruby.exe jruby.bat))
end
