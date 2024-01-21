<!--- Briefly describe your changes in the field above. -->

## Description
<!--- If necessary, go into depth of what this pull request is doing. -->

## Checklist:
<!--- Go over all the following points, and put an `x` in all the boxes that apply. -->
<!--- Please remove whole sections, not points within the sections, that do not apply -->
<!--- If you're unsure about any of these, don't hesitate to ask. We're here to help! -->
- [ ] **I am adding a new extension to a language.**
  - [ ] The new extension is used in hundreds of repositories on GitHub.com
    - Search results for each extension:
      <!-- Replace FOOBAR with the new extension, and KEYWORDS with keywords unique to the language. Repeat for each extension added. -->
      - https://github.com/search?type=code&q=NOT+is%3Afork+path%3A*.FOOBAR+KEYWORDS
  - [ ] I have included a real-world usage sample for all extensions added in this PR:
    - Sample source(s):
      - [URL to each sample source, if applicable]
    - Sample license(s):
  - [ ] I have included a change to the heuristics to distinguish my language from others using the same extension.

- [ ] **I am adding a new language.**
  - [ ] The extension of the new language is used in hundreds of repositories on GitHub.com.
    - Search results for each extension:
      <!-- Replace FOOBAR with the new extension, and KEYWORDS with keywords unique to the language. Repeat for each extension added. -->
      -  https://github.com/search?type=code&q=NOT+is%3Afork+path%3A*.FOOBAR+KEYWORDS
  - [ ] I have included a real-world usage sample for all extensions added in this PR:
    - Sample source(s):
      - [URL to each sample source, if applicable]
    - Sample license(s):
  - [ ] I have included a syntax highlighting grammar: [URL to grammar repo]
      <!-- Setting a color is strongly recommended, but optional: `#cccccc` is used by default -->
  - [ ] I have added a color
    - Hex value: `#RRGGBB`
    - Rationale: <!-- Please specify why you chose this color (if it was randomly selected, please say so); it helps arbitrate future requests to change a language's color -->
  - [ ] I have updated the heuristics to distinguish my language from others using the same extension.

- [ ] **I am fixing a misclassified language**
  - [ ] I have included a new sample for the misclassified language:
    - Sample source(s):
      - [URL to each sample source, if applicable]
    - Sample license(s):
  - [ ] I have included a change to the heuristics to distinguish my language from others using the same extension.

- [ ] **I am changing the source of a syntax highlighting grammar**
  - Old: [URL to grammar repo]
  - New: [URL to grammar repo]

- [ ] **I am updating a grammar submodule**
  <!-- That's not necessary, grammar submodules are updated automatically with each new release. -->

- [ ] **I am adding new or changing current functionality**
  <!-- This includes modifying the vendor, documentation, and generated lists. -->
  - [ ] I have added or updated the tests for the new or changed functionality.

- [ ] **I am changing the color associated with a language**
  <!-- Please ensure you have gathered agreement from the wider language community _before_ opening this PR -->
  - [ ] I have obtained agreement from the wider language community on this color change.
    - [URL to public discussion]
    - [Optional: URL to official branding guidelines for the language]
