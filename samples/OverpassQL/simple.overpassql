// Example comment

/**
 * Multiline comment
 */

[out:json][timeout:25];
{{geocodeArea:Australia}}->.bbox;

(
  way["highway"="motorway"](area.bbox);
  nwr[highway="motorway"][surface!=paved](1, 2, 3, 4);
  rel(bn.a)[highway~motorway]({{bbox}});
);

>;
out meta;