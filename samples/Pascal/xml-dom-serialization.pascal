program CrearXML;

{$mode objfpc}{$H+}

uses
  Classes, XMLWrite, DOM;

var
  xdoc: TXMLDocument;                                  // variable objeto documento XML
  NodoRaiz, NodoPadre, NodoHijo: TDOMNode;             // variables a los nodos
begin
  //crear el documento
  xdoc := TXMLDocument.create;

  NodoRaiz := xdoc.CreateElement('root');               // crear el nodo raíz
  Xdoc.Appendchild(NodoRaiz);                           // guardar nodo raíz
  NodoPadre := xdoc.CreateElement('element');           // crear el nodo hijo
  NodoHijo := xdoc.CreateTextNode('Some text here');    // insertar el valor del nodo
  NodoPadre.Appendchild(NodoHijo);                      // guardar nodo
  NodoRaiz.AppendChild(NodoPadre);                      // insertar el nodo hijo en el correspondiente nodo padre
  writeXMLFile(xDoc,'prueba.xml');                      // escribir el XML
  Xdoc.free;
end.
