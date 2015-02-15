import java.io.IOException;
import java.io.StringReader;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class StudentHandler extends DefaultHandler {
  public static void main(String[] args)throws Exception{
    String xml = "<Students>\n"+
    "<Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"+
    "<Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"+
    "<Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"+
    "<Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"+
    "  <Pet Type=\"dog\" Name=\"Rover\" />\n"+
    "</Student>\n"+
    "<Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"+
    "</Students>";
    StudentHandler handler = new StudentHandler();
    handler.parse(new InputSource(new StringReader(xml)));
  }

  public void parse(InputSource src) throws SAXException, IOException {
		XMLReader parser = XMLReaderFactory.createXMLReader();
    parser.setContentHandler(this);
    parser.parse(src);
  }

  @Override
  public void characters(char[] ch, int start, int length) throws SAXException {
    //if there were text as part of the elements, we would deal with it here
    //by adding it to a StringBuffer, but we don't have to for this task
    super.characters(ch, start, length);
  }

  @Override
  public void endElement(String uri, String localName, String qName) throws SAXException {
    //this is where we would get the info from the StringBuffer if we had to,
    //but all we need is attributes
    super.endElement(uri, localName, qName);
  }

  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
    if(qName.equals("Student")){
      System.out.println(attributes.getValue("Name"));
    }
  }
}
