def inventory = new XmlSlurper().parseText("<inventory...")    //optionally parseText(new File("inv.xml").text)
def firstItem = inventory.section.item[0]                      //1. first item
inventory.section.item.price.each { println it }               //2. print each price
def allNamesArray = inventory.section.item.name.collect {it}   //3. collect item names into an array
