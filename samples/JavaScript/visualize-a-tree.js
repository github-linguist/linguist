<!doctype html>
<html id="doc">
  <head><meta charset="utf-8"/>
    <title>Stuff</title>
    <script type="application/javascript">
	function gid(id) { return document.getElementById(id); }

	function ce(tag, cls, parent_node) {
		var e = document.createElement(tag);
		e.className = cls;
		if (parent_node) parent_node.appendChild(e);
		return e;
	}

	function dom_tree(id) {
		gid('tree').textContent = "";
		gid('tree').appendChild(mktree(gid(id), null));
	}

	function mktree(e, p) {
		var t = ce("div", "tree", p);
		var tog = ce("span", "toggle", t);
		var h = ce("span", "tag", t);

		if (e.tagName === undefined) {
			h.textContent = "#Text";
			var txt = e.textContent;
			if (txt.length > 0 && txt.match(/\S/)) {
				h = ce("div", "txt", t);
				h.textContent = txt;
			}
			return t;
		}

		tog.textContent = "−";
		tog.onclick = function () { clicked(tog); }
		h.textContent = e.nodeName;

		var l = e.childNodes;
		for (var i = 0; i != l.length; i++)
			mktree(l[i], t);
		return t;
	}

	function clicked(e) {
		var is_on = e.textContent == "−";
		e.textContent = is_on ? "+" : "−";
		e.parentNode.className = is_on ? "tree-hide" : "tree";
	}
    </script>
    <style>
      #tree { white-space: pre; font-family: monospace; border: 1px solid }
      .tree > .tree-hide, .tree > .tree
		{ margin-left: 2em; border-left: 1px dotted rgba(0,0,0,.2)}
      .tree-hide > .tree, .tree-hide > .tree-hide { display: none }
      .tag { color: navy }
      .tree-hide > .tag { color: maroon }
      .txt { color: gray; padding: 0 .5em; margin: 0 .5em 0 2em; border: 1px dotted rgba(0,0,0,.1) }
      .toggle { display: inline-block; width: 2em; text-align: center }
    </style>
  </head>
  <body>
    <article>
      <section>
        <h1>Headline</h1>
        Blah blah
      </section>
      <section>
        <h1>More headline</h1>
        <blockquote>Something something</blockquote>
        <section><h2>Nested section</h2>
	  Somethin somethin list:
	  <ul>
	    <li>Apples</li>
	    <li>Oranges</li>
	    <li>Cetera Fruits</li>
	  </ul>
	</section>
      </section>
    </article>
    <div id="tree"><a href="javascript:dom_tree('doc')">click me</a></div>
  </body>
</html>
