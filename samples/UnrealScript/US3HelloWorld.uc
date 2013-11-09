class US3HelloWorld extends GameInfo;
 
event InitGame( string Options, out string Error )
{
    `log( "Hello, world!" );
}

defaultproperties
{
}
