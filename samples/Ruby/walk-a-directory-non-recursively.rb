# Files under this directory:
Dir.glob('*') { |file| puts file }

# Files under path '/foo/bar':
Dir.glob( File.join('/foo/bar', '*') ) { |file| puts file }

# As a method
def file_match(pattern=/\.txt/, path='.')
  Dir[File.join(path,'*')].each do |file|
    puts file if file =~ pattern
  end
end
