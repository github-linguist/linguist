ronn-format(7) -- manual authoring format based on Markdown
===========================================================

## SYNOPSIS

    name(1) -- short, single-sentence description
    =============================================

    ## SYNOPSIS

    `name` [<optional>...] <flags>

    ## DESCRIPTION

    A normal paragraph. This can span multiple lines and is terminated with two
    or more line endings -- just like Markdown.

    Inline markup for `code`, `user input`, and **strong** are displayed
    boldface; <variable>, _emphasis_, *emphasis*, are displayed in italics
    (HTML) or underline (roff).

    Manual references like sh(1), markdown(7), roff(7), etc. are hyperlinked in
    HTML output.

    Link to sections like [STANDARDS][], [SEE ALSO][], or [WITH A DIFFERENT LINK
    TEXT][#SEE-ALSO].

    Definition lists:

      * `-a`, `--argument`=[<value>]:
        One or more paragraphs describing the argument.

      * You can put whatever you *want* here, really:
        Nesting and paragraph spacing are respected.

    Frequently used sections:

    ## OPTIONS
    ## SYNTAX
    ## ENVIRONMENT
    ## RETURN VALUES
    ## STANDARDS
    ## SECURITY CONSIDERATIONS
    ## BUGS
    ## HISTORY
    ## AUTHOR
    ## COPYRIGHT
    ## SEE ALSO

## DESCRIPTION

The ronn(1) command converts text in a simple markup to UNIX manual pages. The
syntax includes all Markdown formatting features, plus conventions for
expressing the structure and various notations present in standard UNIX
manpages.

Not all roff(7) typesetting features can be expressed using ronn syntax.

## MANPAGE TITLE

Manpages have a <name>, <section>, and a one-line <description>. Files must
start with a level one heading defining these attributes:

    ls(1) -- list directory contents
    ================================

Indicates that the manpage is named `ls` in manual section `1` ("user
commands").

## SECTION HEADINGS

Man section headings are expressed with markdown level two headings. There
are two syntaxes for level two headings.

Hash prefix syntax:

    ## HEADING TEXT

Dash underline syntax:

    HEADING TEXT
    ------------

Section headings should be all uppercase and may not contain inline markup.

## INLINE MARKUP

Manpages have a limited set of text formatting capabilities. There's basically
<b>boldface</b> and <i>italics</i> (often displayed using <u>underline</u>).
Ronn uses the following bits of markdown(7) to accomplish this:

  * <code>\`backticks\`</code> (markdown compatible):
    Code, flags, commands, and noun-like things; typically displayed in in
    <b>boldface</b>. All text included within `backticks` is displayed
    literally; other inline markup is not processed. HTML output:
    `<code>`.

  * `**double-stars**` (markdown compatible):
    Also displayed in boldface. Unlike backticks, inline markup is processed.
    HTML output: `<strong>`.

  * `<anglequotes>` (non-compatible markdown extension):
    User-specified arguments, variables, or user input. Typically displayed with
    <u>underline</u> in roff output. HTML output: `<var/>`.

  * `_`_underbars_`_` (markdown compatible):
    Emphasis. May be used for literal option values. Typically displayed with
    <u>underline</u> in roff output. HTML output: `<em>`.

Here is grep(1)'s DESCRIPTION section represented in `ronn`:

    `Grep` searches the named input <FILE> (or standard input if
    no files are named, or the file name `-` is given) for lines
    containing a match to the given <PATTERN>. By default, `grep`
    prints the matching lines.

## DEFINITION LISTS

The definition list syntax is compatible with markdown's unordered list syntax
but requires that the first line of each list item be terminated with a colon
"`:`" character. The contents of the first line is the <term>; subsequent lines
may be comprised of multiple paragraphs, code blocks, standard lists, and nested
definition lists.

An example definition list, taken from BSD test(1)'s *DESCRIPTION* section:

     The following primaries are used to construct expressions:

       * `-b` <file>:
         True if <file> exists and is a block special file.

       * `-c` <file>:
         True if _file_ exists and is a character special file.

       * `-d` <file>:
         True if file exists and is a directory.

## LINKS

All markdown(7) linking features are supported.

Markdown reference-style links can be used to link to specific sections by name:

    ## SECTION 1

    See the following section.

    ## SECTION 2

    See [SECTION 1][] or [to put it another way][SECTION 1].

The anchor name would be `#SECTION-1` and `#SECTION-2`. All non-word characters
are removed and spaces are replaced by dashes.

## SEE ALSO

ronn(1), markdown(7), roff(7)
