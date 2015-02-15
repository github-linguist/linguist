import javax.swing.* ;
import java.awt.* ;

public class Greybars extends JFrame {
   private int width ;
   private int height ;

   public Greybars( )  {
      super( "grey bars example!" ) ;
      width = 640 ;
      height = 320 ;
      setSize( width , height ) ;
      setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE ) ;
      setVisible( true ) ;
    }

    public void paint ( Graphics g ) {
      int run = 0 ;
      int colorcomp = 0 ; //component of the color
      for ( int columncount = 8 ; columncount < 128 ; columncount *= 2 ) {
	 int colorgap = 255 / columncount ; //by this gap we change the background color
	 int columnwidth = width / columncount ;
	 int columnheight = height / 4 ;
	 if ( run % 2 == 0 ) //switches color directions with every for loop
	    colorcomp = 0 ;
	 else {
	    colorcomp = 255 ;
	    colorgap *= -1 ;
	 }
	 int ystart = 0 + columnheight * run ;
	 int xstart = 0 ;
	 for ( int i = 0 ; i < columncount ; i++ ) {
	    Color nextColor = new Color( colorcomp, colorcomp, colorcomp ) ;
	    g.setColor( nextColor ) ;
	    g.fillRect( xstart , ystart , columnwidth , columnheight ) ;
	    xstart += columnwidth ;
	    colorcomp += colorgap ;
	 }
	 run++ ;
      }
    }

    public static void main( String[ ] args ) {
       Greybars gb = new Greybars( ) ;
    }
}
