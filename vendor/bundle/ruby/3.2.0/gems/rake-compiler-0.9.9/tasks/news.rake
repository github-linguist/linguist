desc 'Generate email template to standard output'
task :announce do
  fail "no GEM_SPEC is found or defined. 'announce' task cannot work without it." unless defined?(GEM_SPEC)

  # read project info and overview
  notes = begin
            r = File.read("README.rdoc")
            r.split(/^(=+ .*)/)[1..4].join.strip
          rescue
            warn "Missing README.rdoc"
            ''
          end

  # read changes
  changes = begin
              h = File.read("History.txt")
              h.split(/^(===+ .*)/)[1..2].join.strip
            rescue
              warn "Missing History.txt"
              ''
            end

  # standard fields
  subject = "#{GEM_SPEC.name} #{GEM_SPEC.version} Released"
  title   = "#{GEM_SPEC.name} version #{GEM_SPEC.version} has been released!"
  body    = "#{notes}\n\nChanges:\n\n#{changes}"
  urls    = [GEM_SPEC.homepage].map { |u| "* <#{u.strip}>" }.join("\n")

  puts "=" * 80, ""
  puts "Subject: [ANN] #{subject}"
  puts
  puts title
  puts
  puts urls
  puts
  puts body
  puts
  puts "=" * 80, ""
end
