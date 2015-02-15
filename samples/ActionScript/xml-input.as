package
{
    import flash.display.Sprite;

    public class XMLReading extends Sprite
    {
        public function XMLReading()
        {
            var xml:XML = <Students>
                            <Student Name="April" />
                            <Student Name="Bob" />
                            <Student Name="Chad" />
                            <Student Name="Dave" />
                            <Student Name="Emily" />
                          </Students>;
            for each(var node:XML in xml..Student)
            {
                trace(node.@Name);
            }
        }
    }
}
