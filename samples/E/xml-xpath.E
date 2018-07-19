? def xml__quasiParser := <import:org.switchb.e.xml.makeXMLQuasiParser>()
> def xpath__quasiParser := xml__quasiParser.xPathQuasiParser()
> null

? def doc := xml`<inventory title="OmniCorp Store #45x10^3">
>   <section name="health">
>     <item upc="123456789" stock="12">
>       <name>Invisibility Cream</name>
>       <price>14.50</price>
>       <description>Makes you invisible</description>
>     </item>
>     <item upc="445322344" stock="18">
>       <name>Levitation Salve</name>
>       <price>23.99</price>
>       <description>Levitate yourself for up to 3 hours per application</description>n>
>     </item>
>   </section>
>   <section name="food">
>     <item upc="485672034" stock="653">
>       <name>Blork and Freen Instameal</name>
>       <price>4.95</price>
>       <description>A tasty meal in a tablet; just add water</description>
>     </item>
>     <item upc="132957764" stock="44">
>       <name>Grob winglets</name>
>       <price>3.56</price>
>       <description>Tender winglets of Grob. Just add water</description>
>     </item>
>   </section>
> </inventory>`
# value: xml`...`

? doc[xpath`inventory/section/item`][0]
# value: xml`<item stock="12" upc="123456789">
#              <name>Invisibility Cream</name>
#              <price>14.50</price>
#              <description>Makes you invisible</description>
#            </item>`

? for price in doc[xpath`inventory/section/item/price/text()`] { println(price :String) }
14.50
23.99
4.95
3.56

? doc[xpath`inventory/section/item/name`]
# value: [xml`<name>Invisibility Cream</name>`,
#         xml`<name>Levitation Salve</name>`,
#         xml`<name>Blork and Freen Instameal</name>`,
#         xml`<name>Grob winglets</name>`]
