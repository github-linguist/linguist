import java.util.Random;
import java.util.List;
import java.util.ArrayList;

public class GaltonBox {
    public static void main( final String[] args ) {
        new GaltonBox( 8, 200 ).run();
    }

    private final int        m_pinRows;
    private final int        m_startRow;
    private final Position[] m_balls;
    private final Random     m_random = new Random();

    public GaltonBox( final int pinRows, final int ballCount ) {
        m_pinRows  = pinRows;
        m_startRow = pinRows + 1;
        m_balls    = new Position[ ballCount ];

        for ( int ball = 0; ball < ballCount; ball++ )
            m_balls[ ball ] = new Position( m_startRow, 0, 'o' );
    }

    private static class Position {
        int  m_row;
        int  m_col;
        char m_char;

        Position( final int row, final int col, final char ch ) {
            m_row  = row;
            m_col  = col;
            m_char = ch;
        }
    }

    public void run() {
        for ( int ballsInPlay = m_balls.length; ballsInPlay > 0;  ) {
            ballsInPlay = dropBalls();
            print();
        }
    }

    private int dropBalls() {
        int ballsInPlay = 0;
        int ballToStart = -1;

        // Pick a ball to start dropping
        for ( int ball = 0; ball < m_balls.length; ball++ )
            if ( m_balls[ ball ].m_row == m_startRow )
                ballToStart = ball;

        // Drop balls that are already in play
        for ( int ball = 0; ball < m_balls.length; ball++ )
            if ( ball == ballToStart ) {
                m_balls[ ball ].m_row = m_pinRows;
                ballsInPlay++;
            }
            else if ( m_balls[ ball ].m_row > 0 && m_balls[ ball ].m_row != m_startRow ) {
                m_balls[ ball ].m_row -= 1;
                m_balls[ ball ].m_col += m_random.nextInt( 2 );
                if ( 0 != m_balls[ ball ].m_row )
                    ballsInPlay++;
            }

        return ballsInPlay;
    }

    private void print() {
        for ( int row = m_startRow; row --> 1;  ) {
            for ( int ball = 0; ball < m_balls.length; ball++ )
                if ( m_balls[ ball ].m_row == row )
                    printBall( m_balls[ ball ] );
            System.out.println();
            printPins( row );
        }
        printCollectors();
        System.out.println();
    }

    private static void printBall( final Position pos ) {
        for ( int col = pos.m_row + 1; col --> 0;  )
            System.out.print( ' ' );
        for ( int col = 0; col < pos.m_col; col++ )
            System.out.print( "  " );
        System.out.print( pos.m_char );
    }

    private void printPins( final int row ) {
        for ( int col = row + 1; col --> 0;  )
            System.out.print( ' ' );
        for ( int col = m_startRow - row; col --> 0;  )
            System.out.print( ". " );
        System.out.println();
    }

    private void printCollectors() {
        final List<List<Position>> collectors = new ArrayList<List<Position>>();

        for ( int col = 0; col < m_startRow; col++ ) {
            final List<Position> collector = new ArrayList<Position>();

            collectors.add( collector );
            for ( int ball = 0; ball < m_balls.length; ball++ )
                if ( m_balls[ ball ].m_row == 0 && m_balls[ ball ].m_col == col )
                    collector.add( m_balls[ ball ] );
        }

        for ( int row = 0, rows = longest( collectors ); row < rows; row++ ) {
            for ( int col = 0; col < m_startRow; col++ ) {
                final List<Position> collector = collectors.get( col );
                final int            pos       = row + collector.size() - rows;

                System.out.print( '|' );
                if ( pos >= 0 )
                    System.out.print( collector.get( pos ).m_char );
                else
                    System.out.print( ' ' );
            }
            System.out.println( '|' );
        }
    }

    private static final int longest( final List<List<Position>> collectors ) {
        int result = 0;

        for ( final List<Position> collector : collectors )
            result = Math.max( collector.size(), result );

        return result;
    }
}
