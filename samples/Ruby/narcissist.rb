s = "s = %s%s%s; puts(gets.chomp == (s %% [34.chr, s, 34.chr]) ? 'accept' : 'reject')"; puts(gets.chomp == (s % [34.chr, s, 34.chr]) ? 'accept' : 'reject')
