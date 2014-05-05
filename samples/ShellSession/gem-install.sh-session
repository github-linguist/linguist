$ gem install nokogiri
...
Building native extensions. This could take a while...
...
checking for libxml/parser.h... *** extconf.rb failed ***
Could not create Makefile due to some reason, probably lack of
necessary libraries and/or headers. Check the mkmf.log file for more
details. You may need configuration options.
...
 
$ brew tap homebrew/dupes
Cloning into '/usr/local/Library/Taps/homebrew-dupes'...
remote: Counting objects: 1034, done.
remote: Compressing objects: 100% (591/591), done.
remote: Total 1034 (delta 560), reused 898 (delta 443)
Receiving objects: 100% (1034/1034), 192.53 KiB | 0 bytes/s, done.
Resolving deltas: 100% (560/560), done.
Checking connectivity... done
Warning: Could not tap homebrew/dupes/lsof over mxcl/master/lsof
Tapped 41 formula
 
$ brew install apple-gcc42
==> Downloading http://r.research.att.com/tools/gcc-42-5666.3-darwin11.pkg
######################################################################## 100.0%
==> Caveats
NOTE:
This formula provides components that were removed from XCode in the 4.2
release. There is no reason to install this formula if you are using a
version of XCode prior to 4.2.
 
This formula contains compilers built from Apple's GCC sources, build
5666.3, available from:
 
http://opensource.apple.com/tarballs/gcc
 
All compilers have a `-4.2` suffix. A GFortran compiler is also included.
==> Summary
🍺 /usr/local/Cellar/apple-gcc42/4.2.1-5666.3: 104 files, 75M, built in 11 seconds
 
$ gem install nokogiri -v 1.5.10
Fetching: nokogiri-1.5.10.gem (100%)
Building native extensions. This could take a while...
Successfully installed nokogiri-1.5.10
1 gem installed
Installing ri documentation for nokogiri-1.5.10...
Installing RDoc documentation for nokogiri-1.5.10...

