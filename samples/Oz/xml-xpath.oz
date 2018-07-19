declare
  [XMLParser] = {Module.link ['x-oz://system/xml/Parser.ozf']}

  proc {Main Data}
     Parser = {New XMLParser.parser init}
     [Doc] = {Parser parseVS(Data $)}

     FirstItem = {XPath Doc [inventory section item]}.1

     Prices = {XPath Doc [inventory section item price Text]}

     Names = {XPath Doc [inventory section item name]}
  in
     {ForAll Prices System.showInfo}
  end

  %%
  %% Emulation of some XPath functionality:
  %%

  fun {XPath Doc Path}
     P|Pr = Path
  in
     Doc.name = P %% assert
     {FoldL Pr XPathStep [Doc]}
  end

  fun {XPathStep Elements P}
     if {IsProcedure P} then
        {Map Elements P}
     else
        {FilteredChildren Elements P}
     end
  end

  %% A flat list of all Type-children of all Elements.
  fun {FilteredChildren Elements Type}
     {Flatten
      {Map Elements
       fun {$ E}
	  {Filter E.children
	   fun {$ X}
	      case X of element(name:!Type ...) then true
	      else false
	      end
	   end}
       end}}
  end

  %% PCDATA of an element as a ByteString
  fun {Text Element}
     Texts = for Child in Element.children collect:C do
		case Child of text(data:BS ...) then {C BS} end
	     end
  in
     {FoldR Texts ByteString.append {ByteString.make nil}}
  end

  Data =
   "<inventory title=\"OmniCorp Store #45x10^3\">"
  #"  <section name=\"health\">"
  #"    <item upc=\"123456789\" stock=\"12\">"
  #"      <name>Invisibility Cream</name>"
  #"      <price>14.50</price>"
  #"      <description>Makes you invisible</description>"
  #"    </item>"
  #"    <item upc=\"445322344\" stock=\"18\">"
  #"      <name>Levitation Salve</name>"
  #"      <price>23.99</price>"
  #"      <description>Levitate yourself for up to 3 hours per application</description>"
  #"    </item>"
  #"  </section>"
  #"  <section name=\"food\">"
  #"    <item upc=\"485672034\" stock=\"653\">"
  #"      <name>Blork and Freen Instameal</name>"
  #"      <price>4.95</price>"
  #"      <description>A tasty meal in a tablet; just add water</description>"
  #"    </item>"
  #"    <item upc=\"132957764\" stock=\"44\">"
  #"      <name>Grob winglets</name>"
  #"      <price>3.56</price>"
  #"      <description>Tender winglets of Grob. Just add water</description>"
  #"    </item>"
  #"  </section>"
  #"</inventory>"
in
  {Main Data}
