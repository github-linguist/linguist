def distcheck(n, delta=1)
  unless block_given?
    raise ArgumentError, "pass a block to this method"
  end

  h = Hash.new(0)
  n.times {h[ yield ] += 1}

  target = 1.0 * n / h.length
  h.each do |key, value|
    if (value - target).abs > 0.01 * delta * n
      raise StandardError,
        "distribution potentially skewed for '#{key}': expected around #{target}, got #{value}"
    end
  end

  puts h.sort.map{|k, v| "#{k} #{v}"}
end

if __FILE__ == $0
  begin
    distcheck(100_000) {rand(10)}
    distcheck(100_000) {rand > 0.95}
  rescue StandardError => e
    p e
  end
end
