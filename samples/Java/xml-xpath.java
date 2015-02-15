import java.io.StringReader;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

public class XMLParser {
	final static String xmlStr =
			  "<inventory title=\"OmniCorp Store #45x10^3\">"
			+ "  <section name=\"health\">"
			+ "    <item upc=\"123456789\" stock=\"12\">"
			+ "      <name>Invisibility Cream</name>"
			+ "      <price>14.50</price>"
			+ "      <description>Makes you invisible</description>"
			+ "    </item>"
			+ "    <item upc=\"445322344\" stock=\"18\">"
			+ "      <name>Levitation Salve</name>"
			+ "      <price>23.99</price>"
			+ "      <description>Levitate yourself for up to 3 hours per application</description>"
			+ "    </item>"
			+ "  </section>"
			+ "  <section name=\"food\">"
			+ "    <item upc=\"485672034\" stock=\"653\">"
			+ "      <name>Blork and Freen Instameal</name>"
			+ "      <price>4.95</price>"
			+ "      <description>A tasty meal in a tablet; just add water</description>"
			+ "    </item>"
			+ "    <item upc=\"132957764\" stock=\"44\">"
			+ "      <name>Grob winglets</name>"
			+ "      <price>3.56</price>"
			+ "      <description>Tender winglets of Grob. Just add priwater</description>"
			+ "    </item>"
			+ "  </section>"
			+ "</inventory>";

	public static void main(String[] args) {
		try {
			Document doc = DocumentBuilderFactory.newInstance()
					.newDocumentBuilder()
					.parse(new InputSource(new StringReader(xmlStr)));
			XPath xpath = XPathFactory.newInstance().newXPath();
			// 1
			System.out.println(((Node) xpath.evaluate(
					"/inventory/section/item[1]", doc, XPathConstants.NODE))
					.getAttributes().getNamedItem("upc"));
			// 2, 3
			NodeList nodes = (NodeList) xpath.evaluate(
					"/inventory/section/item/price", doc,
					XPathConstants.NODESET);
			for (int i = 0; i < nodes.getLength(); i++)
				System.out.println(nodes.item(i).getTextContent());
		} catch (Exception e) {
			System.out.println("Error ocurred while parsing XML.");
		}
	}
}
