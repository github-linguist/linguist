require_relative "./helper"

class TestColorThreshold < Minitest::Test
  include Linguist

  def test_color_threshold
    langs_with_colors = Language.all.reject { |language| language.color.nil? }
    cp = ColorProximity.new(10, langs_with_colors.map(&:color))
    failing_threshold = langs_with_colors.map do |lang|
      state = cp.within_threshold?(lang.color[1..-1])
      "- #{lang} (#{lang.color}) is close to #{state.last}" unless state.first
    end.compact
    message = "The following languages have failing color thresholds. Please modify the hex color.\n#{failing_threshold.join("\n")}"

    assert failing_threshold.empty?, message
  end
end
