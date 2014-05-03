(: Query for searching the database for keywords :)

import module namespace index = "http://guide.com/index";
import module namespace catalog = "http://guide.com/catalog";

import module namespace req = "http://www.28msec.com/modules/http-request";

variable $phrase := (req:param-values("q"), "London")[1];
variable $limit := integer((req:param-values("limit"), 5)[1]);

[
for $result at $idx in index:index-search($phrase)
where $idx le $limit
let $data := catalog:get-data-by-id($result.s, $result.p)
return 
    {| { score : $result.r } , $data |}  
]
