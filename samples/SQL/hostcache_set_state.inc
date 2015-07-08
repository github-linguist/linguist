# Helper for hostcache_*.test

# Set a known initial state for the test

flush status;
flush hosts;
flush user_resources;
flush privileges;

# Print critical setup

select @@global.debug;
select @@global.max_connect_errors;
select @@global.max_user_connections;
select @@global.max_connections;

# Make sure there are no remaining records that can change the test outcome

select `User`, `Host` from mysql.`user` where `host` like '%\\%%';
select `User`, `Host` from mysql.`user` where `user` like '192.%';
select `User`, `Host` from mysql.`user` where `user` like '2001:%';
select `User`, `Host` from mysql.`user` where `user` like 'santa.claus.%';

