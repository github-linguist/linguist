scala> val xml = <inventory title="OmniCorp Store #45x10^3">
     |   <section name="health">
     |     <item upc="123456789" stock="12">
     |       <name>Invisibility Cream</name>
     |       <price>14.50</price>
     |       <description>Makes you invisible</description>
     |     </item>
     |     <item upc="445322344" stock="18">
     |       <name>Levitation Salve</name>
     |       <price>23.99</price>
     |       <description>Levitate yourself for up to 3 hours per application</description>
     |     </item>
     |   </section>
     |   <section name="food">
     |     <item upc="485672034" stock="653">
     |       <name>Blork and Freen Instameal</name>
     |       <price>4.95</price>
     |       <description>A tasty meal in a tablet; just add water</description>
     |     </item>
     |     <item upc="132957764" stock="44">
     |       <name>Grob winglets</name>
     |       <price>3.56</price>
     |       <description>Tender winglets of Grob. Just add water</description>
     |     </item>
     |   </section>
     | </inventory>
xml: scala.xml.Elem =
<inventory title="OmniCorp Store #45x10^3">
         <section name="health">
           <item upc="123456789" stock="12">
             <name>Invisibility Cream</name>
             <price>14.50</price>
             <description>Makes you invisible</description>
           </item>
           <item upc="445322344" stock="18">
             <name>Levitation Salve</name>
             <price>23.99</price>
             <description>Levitate yourself for up to 3 hours per application</description>
           </item>
         </section>
         <section name="food">
           <item upc="485672034" stock="653">
             <name>Blork and Freen Instameal</name>
             <price>4.95</price>
             <description>A tasty meal in a tablet; just add water</description>
           </item>
           <item upc="132957764" stock="44">
             <name>Grob winglets</name>
             <price>3.56</price>
             <description>Tender winglets of Grob. Just add water</description>
           </item>
         </section>
       </inventory>

scala> val firstItem = xml \\ "item" head
firstItem: scala.xml.Node =
<item upc="123456789" stock="12">
             <name>Invisibility Cream</name>
             <price>14.50</price>
             <description>Makes you invisible</description>
           </item>

scala> xml \\ "price" foreach println
<price>14.50</price>
<price>23.99</price>
<price>4.95</price>
<price>3.56</price>

scala> xml \\ "price" map (_ text) foreach println
14.50
23.99
4.95
3.56

scala> val elements = xml \\ "name" toArray
elements: Array[scala.xml.Node] = Array(<name>Invisibility Cream</name>, <name>Levitation Salve</name>, <name>Blork and
Freen Instameal</name>, <name>Grob winglets</name>)

scala> val values = xml \\ "name" map (_ text) toArray
values: Array[String] = Array(Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets)
