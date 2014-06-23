#pike __REAL_VERSION__

//! A string wrapper that pretends to be a @[Stdio.File] object
//! in addition to some features of a @[Stdio.FILE] object.


//! This constant can be used to distinguish a FakeFile object
//! from a real @[Stdio.File] object.
constant is_fake_file = 1;

protected string data;
protected int ptr;
protected int(0..1) r;
protected int(0..1) w;
protected int mtime;

protected function read_cb;
protected function read_oob_cb;
protected function write_cb;
protected function write_oob_cb;
protected function close_cb;

//! @seealso
//!   @[Stdio.File()->close()]
int close(void|string direction) {
  direction = lower_case(direction||"rw");
  int cr = has_value(direction, "r");
  int cw = has_value(direction, "w");

  if(cr) {
    r = 0;
  }

  if(cw) {
    w = 0;
  }

  // FIXME: Close callback
  return 1;
}

//! @decl void create(string data, void|string type, void|int pointer)
//! @seealso
//!   @[Stdio.File()->create()]
void create(string _data, void|string type, int|void _ptr) {
  if(!_data) error("No data string given to FakeFile.\n");
  data = _data;
  ptr = _ptr;
  mtime = time();
  if(type) {
    type = lower_case(type);
    if(has_value(type, "r"))
      r = 1;
    if(has_value(type, "w"))
      w = 1;
  }
  else
    r = w = 1;
}

protected string make_type_str() {
  string type = "";
  if(r) type += "r";
  if(w) type += "w";
  return type;
}

//! @seealso
//!   @[Stdio.File()->dup()]
this_program dup() {
  return this_program(data, make_type_str(), ptr);
}

//! Always returns 0.
//! @seealso
//!   @[Stdio.File()->errno()]
int errno() { return 0; }

//! Returns size and the creation time of the string.
Stdio.Stat stat() {
  Stdio.Stat st = Stdio.Stat();
  st->size = sizeof(data);
  st->mtime=st->ctime=mtime;
  st->atime=time();
  return st;
}

//! @seealso
//!   @[Stdio.File()->line_iterator()]
String.SplitIterator line_iterator(int|void trim) {
  if(trim)
    return String.SplitIterator( data-"\r", '\n' );
  return String.SplitIterator( data, '\n' );
}

protected mixed id;

//! @seealso
//!   @[Stdio.File()->query_id()]
mixed query_id() { return id; }

//! @seealso
//!   @[Stdio.File()->set_id()]
void set_id(mixed _id) { id = _id; }

//! @seealso
//!   @[Stdio.File()->read_function()]
function(:string) read_function(int nbytes) {
  return lambda() { return read(nbytes); };
}

//! @seealso
//!   @[Stdio.File()->peek()]
int(-1..1) peek(int|float|void timeout) {
  if(!r) return -1;
  if(ptr >= sizeof(data)) return 0;
  return 1;
}

//! Always returns 0.
//! @seealso
//!   @[Stdio.File()->query_address()]
string query_address(void|int(0..1) is_local) { return 0; }

//! @seealso
//!   @[Stdio.File()->read()]
string read(void|int(0..) len, void|int(0..1) not_all) {
  if(!r) return 0;
  if (len < 0) error("Cannot read negative number of characters.\n");
  int start=ptr;
  ptr += len;
  if(zero_type(len) || ptr>sizeof(data))
    ptr = sizeof(data);

  // FIXME: read callback
  return data[start..ptr-1];
}

//! @seealso
//!   @[Stdio.FILE()->gets()]
string gets() {
  if(!r) return 0;
  string ret;
  sscanf(data,"%*"+(string)ptr+"s%[^\n]",ret);
  if(ret)
  {
    ptr+=sizeof(ret)+1;
    if(ptr>sizeof(data))
    {
      ptr=sizeof(data);
      if(!sizeof(ret))
	ret = 0;
    }
  }

  // FIXME: read callback
  return ret;
}

//! @seealso
//!   @[Stdio.FILE()->getchar()]
int getchar() {
  if(!r) return 0;
  int c;
  if(catch(c=data[ptr]))
    c=-1;
  else
    ptr++;

  // FIXME: read callback
  return c;
}

//! @seealso
//!   @[Stdio.FILE()->unread()]
void unread(string s) {
  if(!r) return;
  if(data[ptr-sizeof(s)..ptr-1]==s)
    ptr-=sizeof(s);
  else
  {
    data=s+data[ptr..];
    ptr=0;
  }
}

//! @seealso
//!   @[Stdio.File()->seek()]
int seek(int pos, void|int mult, void|int add) {
  if(mult)
    pos = pos*mult+add;
  if(pos<0)
  {
    pos = sizeof(data)+pos;
    if( pos < 0 )
	pos = 0;
  }
  ptr = pos;
  if( ptr > strlen( data ) )
      ptr = strlen(data);
  return ptr;
}

//! Always returns 1.
//! @seealso
//!   @[Stdio.File()->sync()]
int(1..1) sync() { return 1; }

//! @seealso
//!   @[Stdio.File()->tell()]
int tell() { return ptr; }

//! @seealso
//!   @[Stdio.File()->truncate()]
int(0..1) truncate(int length) {
  data = data[..length-1];
  return sizeof(data)==length;
}

//! @seealso
//!   @[Stdio.File()->write()]
int(-1..) write(string|array(string) str, mixed ... extra) {
  if(!w) return -1;
  if(arrayp(str)) str=str*"";
  if(sizeof(extra)) str=sprintf(str, @extra);

  if(ptr==sizeof(data)) {
    data += str;
    ptr = sizeof(data);
  }
  else if(sizeof(str)==1)
    data[ptr++] = str[0];
  else {
    data = data[..ptr-1] + str + data[ptr+sizeof(str)..];
    ptr += sizeof(str);
  }

  // FIXME: write callback
  return sizeof(str);
}

//! @seealso
//!   @[Stdio.File()->set_blocking]
void set_blocking() {
  close_cb = 0;
  read_cb = 0;
  read_oob_cb = 0;
  write_cb = 0;
  write_oob_cb = 0;
}

//! @seealso
//!   @[Stdio.File()->set_blocking_keep_callbacks]
void set_blocking_keep_callbacks() { }

//! @seealso
//!   @[Stdio.File()->set_blocking]
void set_nonblocking(function rcb, function wcb, function ccb,
		     function rocb, function wocb) {
  read_cb = rcb;
  write_cb = wcb;
  close_cb = ccb;
  read_oob_cb = rocb;
  write_oob_cb = wocb;
}

//! @seealso
//!   @[Stdio.File()->set_blocking_keep_callbacks]
void set_nonblocking_keep_callbacks() { }


//! @seealso
//!   @[Stdio.File()->set_close_callback]
void set_close_callback(function cb) { close_cb = cb; }

//! @seealso
//!   @[Stdio.File()->set_read_callback]
void set_read_callback(function cb) { read_cb = cb; }

//! @seealso
//!   @[Stdio.File()->set_read_oob_callback]
void set_read_oob_callback(function cb) { read_oob_cb = cb; }

//! @seealso
//!   @[Stdio.File()->set_write_callback]
void set_write_callback(function cb) { write_cb = cb; }

//! @seealso
//!   @[Stdio.File()->set_write_oob_callback]
void set_write_oob_callback(function cb) { write_oob_cb = cb; }


//! @seealso
//!   @[Stdio.File()->query_close_callback]
function query_close_callback() { return close_cb; }

//! @seealso
//!   @[Stdio.File()->query_read_callback]
function query_read_callback() { return read_cb; }

//! @seealso
//!   @[Stdio.File()->query_read_oob_callback]
function query_read_oob_callback() { return read_oob_cb; }

//! @seealso
//!   @[Stdio.File()->query_write_callback]
function query_write_callback() { return write_cb; }

//! @seealso
//!   @[Stdio.File()->query_write_oob_callback]
function query_write_oob_callback() { return write_oob_cb; }

string _sprintf(int t) {
  return t=='O' && sprintf("%O(%d,%O)", this_program, sizeof(data),
			   make_type_str());
}


// FakeFile specials.

//! A FakeFile can be casted to a string.
mixed cast(string to) {
  switch(to) {
  case "string": return data;
  case "object": return this;
  }
  error("Can not cast object to %O.\n", to);
}

//! Sizeof on a FakeFile returns the size of its contents.
int(0..) _sizeof() {
  return sizeof(data);
}

//! @ignore

#define NOPE(X) mixed X (mixed ... args) { error("This is a FakeFile. %s is not available.\n", #X); }
NOPE(assign);
NOPE(async_connect);
NOPE(connect);
NOPE(connect_unix);
NOPE(open);
NOPE(open_socket);
NOPE(pipe);
NOPE(tcgetattr);
NOPE(tcsetattr);

// Stdio.Fd
NOPE(dup2);
NOPE(lock); // We could implement this
NOPE(mode); // We could implement this
NOPE(proxy); // We could implement this
NOPE(query_fd);
NOPE(read_oob);
NOPE(set_close_on_exec);
NOPE(set_keepalive);
NOPE(trylock); // We could implement this
NOPE(write_oob);

//! @endignore