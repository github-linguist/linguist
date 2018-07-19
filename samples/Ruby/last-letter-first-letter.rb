class LastL_FirstL
  def initialize(names)
    @names = names.dup
    @first = names.group_by {|name| name[0]}
    @sequences = []
  end

  def add_name(seq)
    last_letter = seq[-1][-1]
    potentials = @first.include?(last_letter) ? (@first[last_letter] - seq) : []
    if potentials.empty?
      @sequences << seq
    else
      potentials.each {|name| add_name(seq + [name])}
    end
  end

  def search
    @names.each {|name| add_name [name]}
    max = @sequences.max_by {|seq| seq.length}.length
    max_seqs = @sequences.select {|seq| seq.length == max}
    puts "there are #{@sequences.length} possible sequences"
    puts "the longest is #{max} names long"
    puts "there are #{max_seqs.length} such sequences. one is:"
    max_seqs.last.each_with_index {|name, idx| puts "  %2d %s" % [idx+1, name]}
  end
end

names = %w{
  audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
  cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
  girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
  kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
  nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
  porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
  sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
  tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
}

lf = LastL_FirstL.new(names)
lf.search
