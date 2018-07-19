import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ConfigReader {
    private static final Pattern             LINE_PATTERN = Pattern.compile( "([^ =]+)[ =]?(.*)" );
    private static final Map<String, Object> DEFAULTS     = new HashMap<String, Object>() {{
        put( "needspeeling", false );
        put( "seedsremoved", false );
    }};

    public static void main( final String[] args ) {
        System.out.println( parseFile( args[ 0 ] ) );
    }

    public static Map<String, Object> parseFile( final String fileName ) {
        final Map<String, Object> result = new HashMap<String, Object>( DEFAULTS );
        /*v*/ BufferedReader      reader = null;

        try {
            reader = new BufferedReader( new FileReader( fileName ) );
            for ( String line; null != ( line = reader.readLine() );  ) {
                parseLine( line, result );
            }
        } catch ( final IOException x ) {
            throw new RuntimeException( "Oops: " + x, x );
        } finally {
            if ( null != reader ) try {
                reader.close();
            } catch ( final IOException x2 ) {
                System.err.println( "Could not close " + fileName + " - " + x2 );
            }
        }

        return result;
    }

    private static void parseLine( final String line, final Map<String, Object> map ) {
        if ( "".equals( line.trim() ) || line.startsWith( "#" ) || line.startsWith( ";" ) )
            return;

        final Matcher matcher = LINE_PATTERN.matcher( line );

        if ( ! matcher.matches() ) {
            System.err.println( "Bad config line: " + line );
            return;
        }

        final String key   = matcher.group( 1 ).trim().toLowerCase();
        final String value = matcher.group( 2 ).trim();

        if ( "".equals( value ) ) {
            map.put( key, true );
        } else if ( -1 == value.indexOf( ',' ) ) {
            map.put( key, value );
        } else {
            final String[] values = value.split( "," );

            for ( int i = 0; i < values.length; i++ ) {
                values[ i ] = values[ i ].trim();
            }
            map.put( key, Arrays.asList( values ) );
        }
    }
}
