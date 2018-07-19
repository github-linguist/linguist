import java.io.File;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Arrays;
import javax.tools.JavaCompiler;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;

public class Eval {
    private static final String CLASS_NAME = "TempPleaseDeleteMe";

    private static class StringCompiler
            extends SimpleJavaFileObject {
        final String m_sourceCode;

        private StringCompiler( final String sourceCode ) {
            super( URI.create( "string:///" + CLASS_NAME + Kind.SOURCE.extension ), Kind.SOURCE );
            m_sourceCode = sourceCode;
        }

        @Override
        public CharSequence getCharContent( final boolean ignoreEncodingErrors ) {
            return m_sourceCode;
        }

        private boolean compile() {
            final JavaCompiler javac = ToolProvider.getSystemJavaCompiler();

            return javac.getTask( null, javac.getStandardFileManager( null, null, null ),
                null, null, null, Arrays.asList( this )
            ).call();
        }

        private double callEval( final double x )
                throws Exception {
            final Class<?> clarse = Class.forName( CLASS_NAME );
            final Method   eval   = clarse.getMethod( "eval", double.class );

            return ( Double ) eval.invoke( null, x );
        }
    }

    public static double evalWithX( final String code, final double x )
            throws Exception {
        final StringCompiler sc = new StringCompiler(
            "class "
                + CLASS_NAME
                + "{public static double eval(double x){return ("
                + code
                + ");}}"
            );

        if ( ! sc.compile() ) throw new RuntimeException( "Compiler error" );
        return sc.callEval( x );
    }

    public static void main( final String [] args )
            throws Exception /* lazy programmer */ {
        final String expression = args [ 0 ];
        final double x1         = Double.parseDouble( args [ 1 ] );
        final double x2         = Double.parseDouble( args [ 2 ] );

        System.out.println(
            evalWithX( expression, x1 )
            - evalWithX( expression, x2 )
        );
    }
}
