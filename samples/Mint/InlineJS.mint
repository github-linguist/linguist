component Main {
  fun handleClick (event : Html.Event) : Void {
    `alert("Hello")`
  }

  fun render : Html {
    <div onClick={handleClick}>
      <{ "Click to alert!" }>
    </div>
  }
}
