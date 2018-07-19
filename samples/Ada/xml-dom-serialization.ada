with Ada.Text_IO.Text_Streams;
with DOM.Core.Documents;
with DOM.Core.Nodes;

procedure Serialization is
   My_Implementation : DOM.Core.DOM_Implementation;
   My_Document       : DOM.Core.Document;
   My_Root_Node      : DOM.Core.Element;
   My_Element_Node   : DOM.Core.Element;
   My_Text_Node      : DOM.Core.Text;
begin
   My_Document := DOM.Core.Create_Document (My_Implementation);
   My_Root_Node := DOM.Core.Documents.Create_Element (My_Document, "root");
   My_Root_Node := DOM.Core.Nodes.Append_Child (My_Document, My_Root_Node);
   My_Element_Node := DOM.Core.Documents.Create_Element (My_Document, "element");
   My_Element_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Element_Node);
   My_Text_Node := DOM.Core.Documents.Create_Text_Node (My_Document, "Some text here");
   My_Text_Node := DOM.Core.Nodes.Append_Child (My_Element_Node, My_Text_Node);
   DOM.Core.Nodes.Write
     (Stream => Ada.Text_IO.Text_Streams.Stream
        (Ada.Text_IO.Standard_Output),
      N => My_Document,
      Pretty_Print => True);
end Serialization;
