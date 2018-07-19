var langs = ['foo', 'bar', 'baz']; // real list of langs goes here
var end_tag = '</'+'lang>';

var line;
while (line = readline()) {
    line = line.replace(new RegExp('</code>', 'gi'), end_tag);
    for (var i = 0; i < langs.length; i++)
        line = line.replace(new RegExp('<(?:code )?(' + langs[i] + ')>', 'gi'), '<lang $1>')
                   .replace(new RegExp('</' + langs[i] + '>', 'gi'), end_tag);
    print(line);
}
