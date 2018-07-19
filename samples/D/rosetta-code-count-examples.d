import tango.io.Stdout;
import tango.net.http.HttpClient;
import tango.net.http.HttpHeaders;
import tango.text.xml.Document;
import tango.text.Util;

alias HttpHeader.ContentLength CL;

auto url = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml";
void main()
{
    auto client = new HttpClient (HttpClient.Get, url);
    client.open();
    char[] mainData, tmp;
    int total, i;

    void cat(void[] content) { tmp ~= cast(char[]) content; }

    if (client.isResponseOK) {
        client.read(&cat, client.getResponseHeaders.getInt(CL));
        mainData = tmp;
        tmp = null;

        auto doc = new Document!(char);
        doc.parse(mainData);
        foreach (n; doc.query.descendant("cm").attribute("title")) {
            auto subClient = new HttpClient(HttpClient.Get,
                    "http://www.rosettacode.org/w/index.php?title=" ~
                    replace(n.value.dup, ' ', '_') ~ "&action=raw");
            subClient.open();
            if (! subClient.isResponseOK) {
                Stderr (client.getResponse);
                 break;
            }
            subClient.read(&cat, subClient.getResponseHeaders.getInt(CL));
            foreach (segment; patterns(cast(char[])tmp, "=={{header|")) i++;
            --i;
            if (i) --i;
            Stdout.formatln ("{0,-40} - {}", n.value, i);
            total += i;
            tmp = null;
            i = 0;
        }
        Stdout("total examples: ", total).newline;
    } else {
        Stderr (client.getResponse);
    }
}
