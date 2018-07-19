<?php
$dom = new DOMDocument();//the constructor also takes the version and char-encoding as it's two respective parameters
$dom->formatOutput = true;//format the outputted xml
$root = $dom->createElement('root');
$element = $dom->createElement('element');
$element->appendChild($dom->createTextNode('Some text here'));
$root->appendChild($element);
$dom->appendChild($root);
$xmlstring = $dom->saveXML();
