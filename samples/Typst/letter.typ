// This function gets your whole document as its `body`
// and formats it as a simple letter.
#let letter(
  // The letter's sender, which is display at the top of the page.
  sender: none,

  // The letter's recipient, which is displayed close to the top.
  recipient: none,

  // The date, displayed to the right.
  date: none,

  // The subject line.
  subject: none,

  // The name with which the letter closes.
  name: none,

  // The letter's content.
  body
) = {
  // Configure page and text properties.
  set page(margin: (top: 2cm))
  set text(font: "PT Sans")

  // Display sender at top of page. If there's no sender
  // add some hidden text to keep the same spacing.
  text(9pt, if sender == none {
    hide("a")
  } else {
    sender
  })

  v(1.8cm)

  // Display recipient.
  recipient

  v(0.5cm)

  // Display date. If there's no date add some hidden
  // text to keep the same spacing.
  align(right, if date != none {
    date
  } else {
    hide("a")
  })

  v(2cm)

  // Add the subject line, if any.
  if subject != none {
    pad(right: 10%, strong(subject))
  }

  // Add body and name.
  body
  v(1.25cm)
  name
}

#show: letter.with(
  sender: [
    Jane Smith, Universal Exports, 1 Heavy Plaza, Morristown, NJ 07964
  ],
  recipient: [
    Mr. John Doe \
    Acme Corp. \
    123 Glennwood Ave \
    Quarto Creek, VA 22438
  ],
  date: [Morristown, June 9th, 2023],
  subject: [Revision of our Producrement Contract],
  name: [Jane Smith \ Regional Director],
)

Dear Joe,

#lorem(99)

Best,
