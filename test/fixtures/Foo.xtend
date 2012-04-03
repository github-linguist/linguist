package hello.html

import hello.html.dom.Html
import org.junit.Test

class DomExample {
	
	extension DomBuilder db = new DomBuilder
	extension DomSerializer ds = new DomSerializer
	
	@Test
	def void processDom() {
		val x = buildDom
		println(x.serialize)
	}
	
	def buildDom() {
		new Html [
		  head [
		    title [$("XML encoding with Xtend")]
		  ]
		  body [
		    h1 [$("XML encoding with Xtend")]
		    p [$("this format can be used as an alternative to XML.")]
		
		    // an element with attributes and text content
		    a("http://www.xtend-lang.org") [$("Xtend")]
		
		    // mixed content
		    p [
		      $("This is some ") 
		      b[$("mixed")] 
		      $(" text. For more see the ") 
		      a("http://www.xtext.org")[$("Xtext")] 
		      $(" project")
		    ]
		    p [$("More text.")]
		  ]
		]
	}
}