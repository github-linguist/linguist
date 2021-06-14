layout_visits := RECORD
    STRING20 User;
    STRING30 url;
    STRING5 time;
END;
visits := DATASET([ {'Bob', 'www.yahoo.com', '11:30'}, 
                    {'Fred', 'www.amazon.com', '08:30'}, 
                    {'Fred', 'www.amazon.com', '09:30'}, 
                    {'Fred', 'www.amazon.com', '10:30'}, 
                    {'Frank', 'www.amazon.com', '11:31'}, 
                    {'Fred', 'www.amazon.com', '12:30'}, 
                    {'Fred', 'www.amazon.com', '21:30'}, 
                    {'Fred', 'www.cnn.com', '01:30'}, 
                    {'Sara', 'www.yahoo.com', '23:33'}, 
                    {'Bob', 'www.amazon.com', '11:30'}, 
                    {'Bill', 'www.yahoo.com', '07:30'}], layout_visits);

layout_urlInfo := RECORD
    STRING30 url;
    STRING20 category;
    STRING3 pRank;
END;

urlInfo := DATASET([    {'www.yahoo.com', 'all', '1'}, 
                        {'www.cnn.com', 'news', '2'}, 
                        {'www.amazon.com', 'commerce', '3'}, 
                        {'www.lexisnexis.com', 'commerce', '4'}, 
                        {'www.msnbc.com', 'new', '2'}, 
                        {'www.hotwire.com', 'travel', '5'}], layout_urlInfo);

//Distribute Visits by URL, Count visits by URL
layout_visitCounts := RECORD
    visits.url;
    visits_cnt := COUNT(GROUP);
END;

visitcounts := TABLE(   DISTRIBUTE(visits, HASH32(url)), 
                        layout_visitCounts, url, LOCAL);

//Distribute Category by url, JOIN Category to URLs
visitCountsCat := JOIN( visitcounts, 
                        DISTRIBUTE(urlinfo, HASH32(url)), 
                        LEFT.URL = RIGHT.URL, LOCAL);

//Distribute and group by category, Output top 10 URLs for each category
topUrls := TOPN(    GROUP(  DISTRIBUTE(visitCountsCat, HASH32(category)), 
                            category, 
                            ALL, 
                            LOCAL), 
                    10, 
                    -visits_cnt);

OUTPUT(topurls);
