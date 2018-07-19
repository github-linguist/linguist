import std.stdio, std.exception, std.regex, std.algorithm, std.string,
       std.net.curl;

struct YahooResult {
    string url, title, content;

    string toString() const {
        return "\nTitle: %s\nLink:  %s\nText:  %s"
               .format(title, url, content);
    }
}

struct YahooSearch {
    private string query, content;
    private uint page;

    this(in string query_, in uint page_ = 0) {
        this.query = query_;
        this.page = page_;
        this.content = "http://search.yahoo.com/search?p=%s&b=%d"
                       .format(query, page * 10 + 1).get.assumeUnique;
    }

    @property results() const {
        immutable re = `<li>
                          <div \s class="res">
                            <div>
                              <h3>
                                <a \s (?P<linkAttributes> [^>]+)>
                                  (?P<linkText> .*?)
                                </a>
                              </h3>
                            </div>
                            <div \s class="abstr">
                              (?P<abstract> .*?)
                            </div>
                            .*?
                          </div>
                        </li>`;

        const clean = (string s) => s.replace("<[^>]*>".regex("g"),"");

        return content.match(re.regex("gx")).map!(m => YahooResult(
            clean(m.captures["linkAttributes"]
                  .findSplitAfter(`href="`)[1]
                  .findSplitBefore(`"`)[0]),
            clean(m.captures["linkText"]),
            clean(m.captures["abstract"])
        ));
    }

    YahooSearch nextPage() const {
        return YahooSearch(query, page + 1);
    }
}

void main() {
    writefln("%(%s\n%)", "test".YahooSearch.results);
}
