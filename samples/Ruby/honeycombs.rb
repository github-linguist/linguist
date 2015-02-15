Shoes.app(:height => 700, :width => 800) do
  C = Math::cos(Math::PI/3)
  S = Math::sin(Math::PI/3)
  Radius = 60
  letters = [
    %w[L A R N D 1 2],
    %w[G U I Y T 3 4],
    %w[P C F E B 5 6],
    %w[V S O M K 7 8],
    %w[Q X J Z H 9 0],
  ]

  def highlight(hexagon)
    hexagon.style(:fill => magenta)
  end

  def unhighlight(hexagon)
    hexagon.style(:fill => yellow)
  end

  def choose(hexagon)
    hexagon.choose
    highlight hexagon
    chosen = @hexagons.find_all {|h| h.chosen?}.map {|h| h.letter}
    if chosen.size == @hexagons.size
      @chosen.text = 'Every hexagon has been chosen.'
    else
      @chosen.text = "Chosen: #{chosen.sort.join(',')}" +
                     "\nLast Chosen: #{hexagon.letter}"
    end
  end

  width = 20 + (Radius*(7*letters[0].size - 3)/4.0).ceil
  height = 60 + (Radius*(1 + 2*S*letters.size)).ceil
  @hexagons = []
  letter_to_hex = {}

  # create the GUI
  stack(:height => height, :width => width) do
    @chosen = para("Chosen:\nLast chosen:")

    # draw the hexagrams
    letters.size.times do |row|
      letters[0].size.times do |column|
        x = 60 + column * Radius * 0.75 + (1-S)*Radius
        y = 80 + row * Radius * S + (column.odd? ? S * Radius * 0.5 : 0)
        h = shape(x-Radius, y-S*Radius) do
          stroke red
          strokewidth 3
          move_to(x-C*Radius, y-S*Radius)
          line_to(x+C*Radius, y-S*Radius)
          line_to(x+Radius, y)
          line_to(x+C*Radius, y+S*Radius)
          line_to(x-C*Radius, y+S*Radius)
          line_to(x-Radius, y)
          line_to(x-C*Radius, y-S*Radius)
        end

        # add some attributes and methods to the shape
        class << h
          attr_accessor :x, :y, :state, :letter
          def chosen?
            not @state.nil?
          end
          def choose
            @state = :chosen
          end
          def contains?(px,py)
            if @x-Radius < px and px <= @x-C*Radius
              ratio = (px - @x + Radius).to_f/(Radius*(1-C))
              return (@y - ratio*S*Radius < py and py <= @y + ratio*S*Radius)
            elsif @x-C*Radius < px and px <= @x+C*Radius
              return (@y - S*Radius < py and py < @y + S*Radius)
            elsif @x+C*Radius < px and px <= @x+Radius
              ratio = (@x + Radius - px).to_f/(Radius*(1-C))
              return (@y - ratio*S*Radius < py and py <= @y + ratio*S*Radius)
            else
              return false
            end
          end
          def inspect
            '<%s,"%s",%s,%d@%d>' % [self.class, letter, chosen?, x, y]
          end
        end

        h.x = x + x-Radius
        h.y = y + y-S*Radius
        h.letter = letters[row][column]
        unhighlight h

        @hexagons << h
        letter_to_hex[h.letter.downcase] = h
        letter_to_hex[h.letter.upcase] = h

        # add the letter to the hexagon
        para(h.letter) \
          .style(:size => 56, :stroke => red) \
          .move(h.x - C*Radius, h.y - S*Radius)
      end
    end

    # highlight the hexagon under the mouse
    @hex_over = nil
    motion do |x, y|
      hex = @hexagons.find {|h| h.contains?(x,y)}
      unless hex.nil? or hex.chosen?
        highlight hex
      end
      unless @hex_over == hex or @hex_over.nil? or @hex_over.chosen?
        unhighlight @hex_over
      end
      @hex_over = hex
    end

    # handle mouse clicks
    click do |button, x, y|
      info("button #{button} clicked at (#{x}, #{y})")
      hexagon = @hexagons.find {|h| h.contains?(x,y)}
      unless hexagon.nil?
        info("clicked hexagon #{hexagon}")
        choose hexagon
      end
    end

    # handle keystrokes
    keypress do |key|
      if key == "\x11"  # control-Q
        exit
      elsif key == "?"
        info @hexagons.collect {|h| h.inspect}.join("\n")
      elsif letter_to_hex.has_key?(key)
        info("pressed key #{key} -> #{letter_to_hex[key]}")
        choose letter_to_hex[key]
      end
    end
  end
end
