typedef xml TestXML;
typedef html TestHTML;

typedef inCache TestInCache;

Page (int socket) => ();

ReadRequest (int socket) => (int socket, bool close, image_tag *request);

CheckCache (int socket, bool close, image_tag *request) 
	=> (int socket, bool close, image_tag *request);

Handler (int socket, bool close, image_tag *request)
     => (int socket, bool close, image_tag *request);

Complete (int socket, bool close, image_tag *request) => ();

ReadInFromDisk (int socket, bool close, image_tag *request)
     => (int socket, bool close, image_tag *request, __u8 *rgb_data);

Write (int socket, bool close, image_tag *request)
     => (int socket, bool close, image_tag *request);

Compress(int socket, bool close, image_tag *request, __u8 *rgb_data)
	=> (int socket, bool close, image_tag *request);

StoreInCache(int socket, bool close, image_tag *request)
	=> (int socket, bool close, image_tag *request);

Listen () 
	=> (int socket);

source Listen => Page;

Handler:[_, _, inCache]=;
Handler:[_, _, _]=ReadInFromDisk -> Compress -> StoreInCache;

Page = ReadRequest -> CheckCache-> Handler -> Write -> Complete;

atomic CheckCache:{cache};
atomic StoreInCache:{cache};
atomic Complete:{cache};

handle error ReadInFromDisk => FourOhFor;
