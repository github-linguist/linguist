Shoes.app do
  @number = edit_line
  @number.change {update_controls}

  @incr = button('Increment') {update_controls(@number.text.to_i + 1)}
  @decr = button('Decrement') {update_controls(@number.text.to_i - 1)}

  def update_controls(value = @number.text.to_i)
    @number.text = value
    @incr.state = value.to_i >= 10 ? "disabled" : nil
    @decr.state = value.to_i <=  0 ? "disabled" : nil
  end

  update_controls 0
end
