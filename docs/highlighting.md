# Highlighting

GitHub uses Linguist to power the syntax highlighting of code blocks. You know, the difference between this

```
var util = require('util');
var net = require('net');
var stream = require('stream');
var url = require('url');
```

and this

``` js
var util = require('util');
var net = require('net');
var stream = require('stream');
var url = require('url');
```

In order to syntax highlight a code block, you must provide a "language name" to the code block syntax of your Markup of choice.

For example, for Markdown, this would look like:

    ``` javascript

Whereas for AsciiDoc, this might be:

    [source,javascript]

Below is a comprehensive list of of the language codes necessary for syntax highlighting a code block.
