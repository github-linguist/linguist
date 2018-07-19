import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class YahooSearch {
    private String query;
    // Page number
    private int page = 1;
    // Regexp to look for the individual results in the returned page
    private static final Pattern pattern = Pattern.compile(
        "<a class=\"yschttl spt\" href=\"[^*]+?\\*\\*([^\"]+?)\">(.+?)</a></h3>.*?<div class=\"(?:sm-abs|abstr)\">(.+?)</div>");

    public YahooSearch(String query) {
        this.query = query;
    }

    public List<YahooResult> search() throws MalformedURLException, URISyntaxException, IOException {
        // Build the search string, starting with the Yahoo search URL,
        // then appending the query and optionally the page number (if > 1)
        StringBuilder searchUrl = new StringBuilder("http://search.yahoo.com/search?");
        searchUrl.append("p=").append(URLEncoder.encode(query, "UTF-8"));
        if (page > 1) {searchUrl.append("&b=").append((page - 1) * 10 + 1);}
        // Query the Yahoo search engine
        URL url = new URL(searchUrl.toString());
        List<YahooResult> result = new ArrayList<YahooResult>();
        StringBuilder sb = new StringBuilder();
        // Get the search results using a buffered reader
        BufferedReader in = null;
        try {
            in = new BufferedReader(new InputStreamReader(url.openStream()));
            // Read the results line by line
            String line = in.readLine();
            while (line != null) {
                sb.append(line);
                line = in.readLine();
            }
        }
        catch (IOException ioe) {
            ioe.printStackTrace();
        }
        finally {
            try {in.close();} catch (Exception ignoreMe) {}
        }
        String searchResult = sb.toString();
        // Look for the individual results by matching the regexp pattern
        Matcher matcher = pattern.matcher(searchResult);
        while (matcher.find()) {
            // Extract the result URL, title and excerpt
            String resultUrl = URLDecoder.decode(matcher.group(1), "UTF-8");
            String resultTitle = matcher.group(2).replaceAll("</?b>", "").replaceAll("<wbr ?/?>", "");
            String resultContent = matcher.group(3).replaceAll("</?b>", "").replaceAll("<wbr ?/?>", "");
            // Create a new YahooResult and add to the list
            result.add(new YahooResult(resultUrl, resultTitle, resultContent));
        }
        return result;
    }

    public List<YahooResult> search(int page) throws MalformedURLException, URISyntaxException, IOException {
        // Set the page number and search
        this.page = page;
        return search();
    }

    public List<YahooResult> nextPage() throws MalformedURLException, URISyntaxException, IOException {
        // Increment the page number and search
        page++;
        return search();
    }

    public List<YahooResult> previousPage() throws MalformedURLException, URISyntaxException, IOException {
        // Decrement the page number and search; if the page number is 1 return an empty list
        if (page > 1) {
            page--;
            return search();
        } else return new ArrayList<YahooResult>();
    }
}

class YahooResult {
    private URL url;
    private String title;
    private String content;

    public URL getUrl() {
        return url;
    }

    public void setUrl(URL url) {
        this.url = url;
    }

    public void setUrl(String url) throws MalformedURLException {
        this.url = new URL(url);
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public YahooResult(URL url, String title, String content) {
        setUrl(url);
        setTitle(title);
        setContent(content);
    }

    public YahooResult(String url, String title, String content) throws MalformedURLException {
        setUrl(url);
        setTitle(title);
        setContent(content);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (title != null) {
            sb.append(",title=").append(title);
        }
        if (url != null) {
            sb.append(",url=").append(url);
        }
        return sb.charAt(0) == ',' ? sb.substring(1) : sb.toString();
    }
}

public class TestYahooSearch {
    public static void main(String[] args) throws MalformedURLException, URISyntaxException, IOException {
        // Create a new search
        YahooSearch search = new YahooSearch("Rosetta code");
        // Get the search results
        List<YahooResult> results = search.search();
        // Show the search results
        for (YahooResult result : results) {
            System.out.println(result.toString());
        }
    }
}
