import java.io.*;

public class StripLineComments{
    public static void main( String[] args ){
	if( args.length < 1 ){
	    System.out.println("Usage: java StripLineComments StringToProcess");
	}
	else{
	    String inputFile = args[0];
	    String input = "";
	    try{
		BufferedReader reader = new BufferedReader( new FileReader( inputFile ) );
		String line = "";
		while((line = reader.readLine()) != null){
		    System.out.println( line.split("[#;]")[0] );
		}
	    }
	    catch( Exception e ){
		e.printStackTrace();
	    }
	}
    }
}
