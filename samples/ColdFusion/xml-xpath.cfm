<cfsavecontent variable="xmlString">
<inventory
...
</inventory>
</cfsavecontent>
<cfset xml = xmlParse(xmlString)>
<!--- First Task --->
<cfset itemSearch = xmlSearch(xml, "//item")>
<!--- item = the first Item (xml element object) --->
<cfset item = itemSearch[1]>
<!--- Second Task --->
<cfset priceSearch = xmlSearch(xml, "//price")>
<!--- loop and print each price --->
<cfloop from="1" to="#arrayLen(priceSearch)#" index="i">
  #priceSearch[i].xmlText#<br/>
</cfloop>
<!--- Third Task --->
<!--- array of all the name elements --->
<cfset names = xmlSearch(xml, "//name")>
<!--- visualize the results --->
<cfdump var="#variables#">
