/*
Using the Qt library's XML parser.
*/
#include <iostream>

#include <QDomDocument>
#include <QObject>

int main() {
    QDomDocument doc;

    doc.setContent(
       QObject::tr(
          "<Students>\n"
          "<Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
          "<Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
          "<Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
          "<Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
          "<Pet Type=\"dog\" Name=\"Rover\" />\n"
          "</Student>\n"
          "<Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
          "</Students>"));

    QDomElement n = doc.documentElement().firstChildElement("Student");
    while(!n.isNull()) {
        std::cout << qPrintable(n.attribute("Name")) << std::endl;
        n = n.nextSiblingElement();
    }
    return 0;
}
