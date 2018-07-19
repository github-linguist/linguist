#include <vector>
#include <string>
#include <iostream>
#include <boost/cstdint.hpp>
#include <boost/thread.hpp>
#include <boost/thread/locks.hpp>
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>

typedef boost::mutex Fork;
typedef boost::shared_ptr< Fork > ForkPtr;
typedef boost::lock_guard< Fork > ForkLock;

#define MIN_WAIT_TIME 100
#define NUM_MEALS     10
#define MAX_JITTER    50

template< typename Stream >
class AtomicLogger {
public:

  AtomicLogger( Stream& stream ) :
    m_mutex(),
    m_stream( stream )
  {
  }

  void log( const std::string& str ) {
    boost::mutex::scoped_lock lock( m_mutex );
    m_stream << str << std::endl;
  }

private:
  mutable boost::mutex m_mutex;
  Stream& m_stream;
};
typedef AtomicLogger< std::ostream > AtomicLoggerOstream;
typedef boost::shared_ptr< AtomicLoggerOstream > AtomicLoggerOstreamPtr;

class Philosopher {
public:

  Philosopher(
	      const std::string& name,
	      ForkPtr fork_left,
	      ForkPtr fork_right,
	      AtomicLoggerOstreamPtr p_logger ) :
    m_name( name ),
    m_continue( true ),
    mp_fork_left( fork_left ),
    mp_fork_right( fork_right ),
    m_thread( boost::thread( boost::bind( &Philosopher::thread_func,
					  this,
					  &m_continue,
					  mp_fork_left,
					  mp_fork_right ) ) ),
    m_meals_left( NUM_MEALS ),
    mp_logger( p_logger )
  {
  }

  ~Philosopher() {
    done_dining();
    wait_for_cmplt();
  }

  void done_dining() { m_continue = false; }

  void wait_for_cmplt() { m_thread.join(); }

private:
  inline bool can_grab_fork( ForkPtr& p_fork ) { return p_fork->try_lock(); }

  void thread_func( volatile bool* p_continue, ForkPtr fork_left, ForkPtr fork_right ) {
    bool failed_to_grab_fork = false;

    while( p_continue && m_meals_left ) {
      mp_logger->log( boost::str( boost::format( "%1% is thinking" ) % this->m_name ) );
      wait();
      mp_logger->log( boost::str( boost::format( "%1% is hungry" ) % this->m_name ) );

      // attempt to grab forks
      if( can_grab_fork( fork_left ) ) {
	ForkLock lock_left( *fork_left, boost::adopt_lock );
	if( can_grab_fork( fork_right ) ) {
	  ForkLock lock_right( *fork_right, boost::adopt_lock );
	  // eating
	  mp_logger->log( boost::str( boost::format( "%1% is eating (%2%)..." ) % m_name % m_meals_left ) );
	  wait();
	  // record the meal
	  --m_meals_left;
	} else {
	  failed_to_grab_fork = true;
	}
      } else {
	failed_to_grab_fork = true;
      }

      if( failed_to_grab_fork ) {
	mp_logger->log( boost::str( boost::format( "%1% couldn't get forks; waiting..." ) % m_name ) );
	failed_to_grab_fork = false;
	wait();
      }
    }

    mp_logger->log( boost::str( boost::format( "%1% is done dining" ) % m_name ) );
  }
	
  inline void wait() {
    wait( MIN_WAIT_TIME + ( std::rand() % MAX_JITTER ) );
  }
	
  inline void wait( boost::uint32_t time_in_ms ) {
    boost::this_thread::sleep( boost::posix_time::milliseconds( time_in_ms ) );
  }
	
  std::string m_name;
  volatile bool m_continue;
  ForkPtr mp_fork_left;  // must be declared before the thread
  ForkPtr mp_fork_right; // must be declared before the thread
  boost::thread m_thread;
  boost::uint32_t m_meals_left;
  AtomicLoggerOstreamPtr mp_logger;
};
typedef boost::shared_ptr< Philosopher > PhilosopherPtr;

int main() {
  const int N = 5;
  std::string names[] = { "Aristotle", "Spinoza", "Russell", "Kant", "Plato" };

  std::vector< PhilosopherPtr > philosophers;
  philosophers.reserve( N );

  // create logger
  AtomicLoggerOstreamPtr p_logger( new AtomicLoggerOstream( std::cout ) );

  // create forks
  std::vector< ForkPtr > forks;
  forks.reserve( N );
  for( int i = 0; i < N; ++i ) {
    forks.push_back( ForkPtr( new Fork() ) );
  }

  // create philosophers
  for( int i = 0; i < N; ++i ) {
    philosophers.push_back( PhilosopherPtr(
					   new Philosopher( names[ i ], forks[ i ], forks[ (i + 1) % N ], p_logger ) ) );
  }

  // wait for them to finish
  for( int i = 0; i < N; ++i ) {
    philosophers[ i ]->wait_for_cmplt();
  }

  p_logger->log( "Everyone is done dining." );

  return 0;
}
