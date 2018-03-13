// concrete node signatures
Listen ()
  => (int socket);

ReadRequest (int socket)
  => (int socket, bool close, image_tag *request);

CheckCache (int socket, bool close, image_tag *request)
  => (int socket, bool close, image_tag *request);

// omitted for space:
// ReadInFromDisk, StoreInCache
Compress (int socket, bool close, image_tag *request, __u8 *rgb_data)
  => (int socket, bool close, image_tag *request);
Write (int socket, bool close, image_tag *request)
  => (int socket, bool close, image_tag *request);
Complete (int socket, bool close, image_tag *request) => ();

// source node
source Listen => Image;

// abstract node
Image = ReadRequest -> CheckCache -> Handler -> Write -> Complete;

// predicate type & dispatch
typedef hit TestInCache;
Handler:[_, _, hit] = ;
Handler:[_, _, _] =
ReadInFromDisk -> Compress -> StoreInCache;

// error handler
handle error ReadInFromDisk => FourOhFor;

// atomicity constraints
atomic CheckCache:{cache};
atomic StoreInCache:{cache};
atomic Complete:{cache};

