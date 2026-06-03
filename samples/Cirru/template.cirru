doctype

html
  head
    title "From Java to Kotlin"
    meta (:charset utf-8)
    link (:rel stylesheet) (:href css/style.css)
    link (:rel stylesheet) (:href css/highlightjs-github.css)
    script (:src js/highlight.9.4.0.js)
    script "hljs.initHighlightingOnLoad();"

  body
    a
      :href
        = https://github.com/fabiomsr/from-java-to-kotlin
      :class
        = github-corner
      :aria-label
        = View source on Github

    style
      = ".github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}"

    #note
      = "From Java to Kotlin"

    ul
      li
        a
          :class
            = selected
          :href
            = index.html
          = Basic
      li
        a
          :href
            = functions.html
          = Functions
      li
        a
          :href
            = classes.html
          = Classes


    .section
      .title BASICS
      .case (.name "Print") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/print.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/print.kt)
      .case (.name "Variables I") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/variables-i.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/variables-i.kt)
      .case (.name "Variables II") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/variables-ii.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/variables-ii.kt)
      .case (.name "Null I") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/null-i.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/null-i.kt)

    .section
      .title BASICS
      .case (.name "Bits Operations") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/bits-operations.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/bits-operations.kt)
      .case (.name "Is As In") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/operations.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/operations.kt)
      .case (.name "Smart Cast") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/cast.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/cast.kt)
      .case (.name "Switch / When") $ .pair
        .card (.lang Java) $ pre.code $ code (@insert ../code/java/basic/switch.java)
        .card (.lang Kotlin) $ pre.code $ code (@insert ../code/kotlin/basic/when.kt)
