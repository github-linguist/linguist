<?php
//PHP5 only example due to changes in XML extensions between version 4 and 5 (Tested on PHP5.2.0)
$doc = DOMDocument::loadXML('<inventory title="OmniCorp Store #45x10^3">...</inventory>');
//Load from file instead with $doc = DOMDocument::load('filename');
$xpath = new DOMXPath($doc);
/*
    1st Task: Retrieve the first "item" element
*/
$nodelist = $xpath->query('//item');
$result = $nodelist->item(0);
/*
    2nd task: Perform an action on each "price" element (print it out)
*/
$nodelist = $xpath->query('//price');
for($i = 0; $i < $nodelist->length; $i++)
{
  //print each price element in the DOMNodeList instance, $nodelist, as text/xml followed by a newline
  print $doc->saveXML($nodelist->item($i))."\n";
}
/*
    3rd Task: Get an array of all the "name" elements
*/
$nodelist = $xpath->query('//name');
//our array to hold all the name elements, though in practice you'd probably not need to do this and simply use the DOMNodeList
$result = array();
//a different way of iterating through the DOMNodeList
foreach($nodelist as $node)
{
  $result[] = $node;
}
