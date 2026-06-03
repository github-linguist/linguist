component TodoItem {
  property color = "#333"
  property label = ""
  property done = false

  style base {
    align-items: center;
    display: flex;
  }

  style label {
    font-weight: bold;
    color: #{color};
    flex: 1;

    if (done) {
      text-decoration: line-through;
    }
  }

  fun render {
    <div::base>
      <span::label>
        <{ label }>
      </span>

      <Icon.Checkmark/>
      <Icon.Trash/>
    </div>
  }
}
