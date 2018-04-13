require_relative "./grammar_list"
require_relative "./grammar_source"
require_relative "./host"
require_relative "./submodule"
require "open3"

$quiet = false

# Print debugging feedback to STDOUT if $verbose global is set
def log(msg)
  puts msg unless $quiet
end

def command(*args)
  log "$ #{args.join(' ')}"
  output, status = Open3.capture2e(*args)
  if !status.success?
    output.each_line do |line|
      log "  > #{line}"
    end
    warn "Command failed. Aborting."
    exit 1
  end
end


ROOT = File.expand_path "../../../", __FILE__

# Expand a file path relative to Linguist's base directory
def repo_path(path)
  path = path.sub /^#{Regexp.escape ROOT}\/?/, ""
  "#{ROOT}/#{path}"
end

def exists?(path)
  File.exist? repo_path(path)
end

def read(path)
  File.read repo_path(path)
end

def write(path, data)
  File.write repo_path(path), data
end
