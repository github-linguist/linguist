httpism = require 'httpism'
async = require 'async'
resolve = require 'url'.resolve

exports.squash (url) ! =
    html = httpism.get ! (url).body
    squash html ! (html, url)

squash html (html, url, callback) =
    replacements = sort (links in (html).concat(scripts in (html)))
    for each @(r) in (replacements) @{ r.url = resolve(url, r.href) }
    async.map (replacements, get) @(err, requested)
        callback (err, replace (requested) in (html))    

sort (replacements) =
    replacements.sort @(a, b) @{ a.index - b.index }

get (replacement) =
    replacement.body = httpism.get ! (replacement.url).body
    replacement

replace (replacements) in (html) =
    i = 0
    parts = ""
    for each @(rep) in (replacements)
        parts := "#(parts)#(html.substring(i, rep.index))<#(rep.tag)>#(rep.body)</#(rep.tag)>"
        i := rep.index + rep.length
    
    parts + html.substr(i)

links in (html) =
    link reg = r/<link\s[^>]*href=["']?([^"']+)["'][^\>]*(\/\>|\>\s*\<\/link\>)/gi
    elements in (html) matching (link reg) as 'style'

scripts in (html) =
    script reg = r/<script\s[^>]*src=["']?([^"']+)["'][^\>]*(\/\>|\>\s*\<\/script\>)/gi
    elements in (html) matching (script reg) as 'script'

elements in (html) matching (reg) as (tag) =
    elements = []
    while (m = reg.exec (html))
        elements.push { tag = tag, index = m.index, length = m.0.length, href = m.1 }
    
    elements
