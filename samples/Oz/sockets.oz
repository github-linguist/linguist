declare
  Socket = {New Open.socket init}
in
  {Socket connect(port:256)}
  {Socket write(vs:"hello socket world")}
  {Socket close}
