import kxml.xml;
char[][][]characters =
        [["April","Bubbly: I'm > Tam and <= Emily"],
        ["Tam O'Shanter","Burns: \"When chapman billies leave the street ...\""],
        ["Emily","Short & shrift"]];
void addChars(XmlNode root,char[][][]info) {
        auto remarks = new XmlNode("CharacterRemarks");
        root.addChild(remarks);
        foreach(set;info) {
                remarks.addChild((new XmlNode("Character")).setAttribute("name",set[0]).addCData(set[1]));
        }
}
void main() {
        auto root = new XmlNode("");
        root.addChild(new XmlPI("xml"));
        addChars(root,characters);
        std.stdio.writefln("%s",root.write);
}
