desc 'Package gems and upload to RubyGems'
task :release, [:version] => [:package] do |t, args|
  args.with_defaults(:version => "")
  ver = args.version

  fail "no GEM_SPEC is found or defined. 'release' task cannot work without it." unless defined?(GEM_SPEC)

  # compare versions to avoid mistakes
  unless ver == GEM_SPEC.version.to_s then
    fail "Version mismatch (supplied and specification versions differ)."
  end

  files = FileList["pkg/#{GEM_SPEC.name}-#{GEM_SPEC.version}*.*"].to_a
  fail "No files found for the release." if files.empty?

  puts "Files to release:"
  files.each do |f|
    puts "  * #{f}"
  end

  puts "Releasing #{GEM_SPEC.name} version #{GEM_SPEC.version}..."
  files.each do |f|
    system "gem push #{f}"
  end
  puts "Done."
end
