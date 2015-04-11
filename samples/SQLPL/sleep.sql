create procedure sleep (in sleeptime integer)
begin
  declare wait_until timestamp;

  set wait_until = (current timestamp + sleeptime seconds);
  while (wait_until > current timestamp)
    do
    end while;
end!
