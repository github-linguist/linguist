#lang at-exp racket

(define input @~a{
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
  </inventory>})

(require xml xml/path)

(define data (xml->xexpr
              ((eliminate-whitespace '(inventory section item))
               (read-xml/element (open-input-string input)))))

;; Retrieve the first "item" element
(displayln (xexpr->string (se-path* '(item) data)))
;; => <name>Invisibility Cream</name>

;; Perform an action on each "price" element (print it out)
(printf "Prices: ~a\n" (string-join (se-path*/list '(item price) data) ", "))
;; => Prices: 14.50, 23.99, 4.95, 3.56

;; Get an array of all the "name" elements
(se-path*/list '(item name) data)
;; => '("Invisibility Cream" "Levitation Salve" "Blork and Freen Instameal" "Grob winglets")
