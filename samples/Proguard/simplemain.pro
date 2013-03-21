# taken from the examples page from the user manual located at http://proguard.sourceforge.net/#manual/examples.html.

-injars       myapplication.jar
-outjars      myapplication_out.jar
-libraryjars  <java.home>/lib/rt.jar
-printmapping myapplication.map

-keep public class mypackage.MyMain {
	    public static void main(java.lang.String[]);
}
