require 'lxp'
data = [[<inventory title="OmniCorp Store #45x10^3">
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
</inventory>]]
local first = true
local names, prices = {}, {}
p = lxp.new({StartElement = function (parser, name)
	local a, b, c = parser:pos() --line, offset, pos
	if name == 'item' and first then
		print(data:match('.-</item>', c - b + 1))
		first = false
	end
	if name == 'name' then names[#names+1] = data:match('>(.-)<', c) end
	if name == 'price' then prices[#prices+1] = data:match('>(.-)<', c) end
end})

p:parse(data)
p:close()

print('Name: ', table.concat(names, ', '))
print('Price: ', table.concat(prices, ', '))
