NB. Load the socket libraries

  load 'socket'
  coinsert 'jsocket'

NB. fetch and implicitly display the hostname

  > {: sdgethostname ''

NB. If fetching the hostname is the only reason for loading the socket libraries,
NB. and the hostname is fetched only once, then use a 'one-liner' to accomplish it:

  > {: sdgethostname coinsert 'jsocket' [ load 'socket'
