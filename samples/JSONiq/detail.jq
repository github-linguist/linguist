(: Query for returning one database entry :) 

import module namespace req = "http://www.28msec.com/modules/http-request";
import module namespace catalog = "http://guide.com/catalog";

variable $id := (req:param-values("id"), "London")[1];
variable $part := (req:param-values("part"), "main")[1];

catalog:get-data-by-key($id, $part)
