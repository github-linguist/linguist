Shoes.app do
  stack do
    @textbox = edit_line

    @textbox.change do
      @textbox.text = @textbox.text.gsub(/[^\d]/, '') and alert "Input must be a number!" if @textbox.text !~ /^\d*$/
    end

    flow do
      button "Increment" do
        @textbox.text = @textbox.text.to_i + 1
      end

      button "Random" do
        @textbox.text = rand 5000 if confirm "Do you want a random number?"
      end
    end
  end
end
