require_relative "./helper"

class TestColorThreshold < Minitest::Test
  include Linguist

  def test_color_threshold
    langs_with_colors = Language.all.reject { |language| language.color.nil? }
    cp = ColorProximity.new(20, langs_with_colors.map(&:color))
    failing_threshold = []
    langs_with_colors.each do |lang|
      state = cp.within_threshold?(lang.color[1..-1])
      unless state.first
        failing_threshold << [lang, state.last]
      end
    end
    message = "The following languages have failing color thresholds. Please modify the hex color.\n#{failing_threshold}"

    assert failing_threshold.empty?, message
  end
end
