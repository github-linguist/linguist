// This function gets your whole document as its `body` and formats
// it as a simple fiction book.
#let book(
  // The book's title.
  title: "Book title",

  // The book's author.
  author: "Author",

  // The paper size to use.
  paper: "iso-b5",

  // A dedication to display on the third page.
  dedication: none,

  // Details about the book's publisher that are
  // display on the second page.
  publishing-info: none,

  // The book's content.
  body,
) = {
  // Set the document's metadata.
  set document(title: title, author: author)

  // Set the body font. TeX Gyre Pagella is a free alternative
  // to Palatino.
  set text(font: "TeX Gyre Pagella")

  // Configure the page properties.
  set page(
    paper: paper,
    margin: (bottom: 1.75cm, top: 2.25cm),
  )

  // The first page.
  page(align(center + horizon)[
    #text(2em)[*#title*]
    #v(2em, weak: true)
    #text(1.6em, author)
  ])

  // Display publisher info at the bottom of the second page.
  if publishing-info != none {
    align(center + bottom, text(0.8em, publishing-info))
  }

  pagebreak()

  // Display the dedication at the top of the third page.
  if dedication != none {
    v(15%)
    align(center, strong(dedication))
  }

  // Books like their empty pages.
  pagebreak()
  pagebreak()

  // Configure paragraph properties.
  set par(leading: 0.78em, first-line-indent: 12pt, justify: true)
  show par: set block(spacing: 0.78em)

  // Start with a chapter outline.
  outline(title: [Chapters])

  // Configure page properties.
  set page(
    numbering: "1",

    // The header always contains the book title on odd pages and
    // the chapter title on even pages, unless the page is one
    // the starts a chapter (the chapter title is obvious then).
    header: locate(loc => {
      // Are we on an odd page?
      let i = counter(page).at(loc).first()
      if calc.odd(i) {
        return text(0.95em, smallcaps(title))
      }

      // Are we on a page that starts a chapter? (We also check
      // the previous page because some headings contain pagebreaks.)
      let all = query(heading, loc)
      if all.any(it => it.location().page() in (i - 1, i)) {
        return
      }

      // Find the heading of the section we are currently in.
      let before = query(heading, before: loc)
      if before != () {
        align(right, text(0.95em, smallcaps(before.last().body)))
      }
    }),
  )

  // Configure chapter headings.
  show heading.where(level: 1): it => {
    // Always start on even pages.
    pagebreak()
    counter(page).display(i => if calc.odd(i) {
      pagebreak()
    })

    // Create the heading numbering.
    let number = if it.numbering != none {
      counter(heading).display(it.numbering)
      h(7pt, weak: true)
    }

    v(5%)
    text(2em, weight: 700, block([#number #it.body]))
    v(1.25em)
  }
  show heading: set text(11pt, weight: 400)

  body
}

#show: book.with(
  title: "Liam's Playlist",
  author: "Janet Doe",
  dedication: [for Rachel],
  publishing-info: [
    UK Publishing, Inc. \
    6 Abbey Road \
    Vaughnham, 1PX 8A3

    #link("https://example.co.uk/")

    971-1-XXXXXX-XX-X
  ],
)

= Mondays
Liam hated Mondays. He hated waking up to the sound of his dad's old car sputtering to life outside his window. He hated the smell of burnt toast and instant coffee that filled the kitchen. He hated the sight of his mum's tired face as she handed him his lunch bag and kissed him goodbye. He hated the feel of his worn-out uniform and backpack as he walked to the bus stop. He hated the noise of the other kids on the bus, talking about their weekend plans and their latest crushes. He hated the fact that he had nothing to say to them, nothing to share, nothing to look forward to.

He got off the bus at his school and made his way to his locker, avoiding eye contact with anyone who might notice him or worse, pick on him. He was used to being invisible, being ignored, being alone. He didn't have any friends at school, or anywhere else for that matter. He didn't have any hobbies or interests that made him stand out or fit in. He didn't have any dreams or goals that gave him hope or motivation. He just had his routine: wake up, go to school, come home, do homework, watch TV, go to bed. Repeat.

He opened his locker and took out his books for his first class: English literature. He liked reading books sometimes, but he didn't like analyzing them or writing essays about them. He didn't see the point of studying something that had no relevance to his life or future. What did Shakespeare or Dickens have to do with him? What did he care about metaphors or themes or symbols? He just wanted to escape into a different world for a while, not dissect it.

He closed his locker and headed to class. As he walked down the hall, he saw her: Alice Walker. She was new at school this year and she was beautiful. She had long blonde hair that cascaded over her shoulders like a waterfall. She had bright blue eyes that sparkled like diamonds in the sunlight. She had a perfect smile that lit up her face like a star in the night sky.

But he knew it was impossible. She was out of his league. She was from another world. He sighed and continued walking towards English literature. He hated Mondays.

= Music
#lorem(1500)

= Magic
#lorem(600)

/*
= Mystery
#lorem(600)

= Money
#lorem(6000)

= Mistakes
#lorem(6000)

= Memory
#lorem(6000)

= Miracle
#lorem(6000)

= Monday again
#lorem(6000)
