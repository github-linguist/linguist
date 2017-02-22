typedef choke TestChoke;
typedef unchoke TestUnchoke;
typedef interested TestInterested;
typedef uninterested TestUninterested;
typedef request TestRequest;
typedef cancel TestCancel;
typedef piece TestPiece;
typedef bitfield TestBitfield;
typedef have TestHave;
typedef piececomplete TestPieceComplete;

CheckinWithTracker (torrent_data_t *tdata)
    => ();

SendRequestToTracker (torrent_data_t *tdata)
    => (torrent_data_t *tdata, int socket);
    
GetTrackerResponse (torrent_data_t *tdata, int socket)
    => ();

UpdateChokeList (torrent_data_t *tdata)
    => ();

PickChoked (torrent_data_t *tdata)
    => (torrent_data_t *tdata, chokelist_t clist);
    
SendChokeUnchoke (torrent_data_t *tdata, chokelist_t clist)
    => ();
    
SetupConnection (torrent_data_t *tdata, int socket)
    => ();
    
Handshake (torrent_data_t *tdata, int socket)
    => (torrent_data_t *tdata, client_data_t *client);
    
SendBitfield (torrent_data_t *tdata, client_data_t *client)
    => ();

Message (torrent_data_t *tdata, client_data_t *client)
    => ();
    
ReadMessage (torrent_data_t *tdata, client_data_t *client)
    => (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload);
    
HandleMessage (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);
    
MessageDone (client_data_t *client)
    => ();
    
CompletePiece (torrent_data_t *tdata, client_data_t *client, int piece)
    => (torrent_data_t *tdata, client_data_t *client);

VerifyPiece (torrent_data_t *tdata, client_data_t *client, int piece)
    => (torrent_data_t *tdata, client_data_t *client, int piece);

SendHave (torrent_data_t *tdata, client_data_t *client, int piece)
    => (torrent_data_t *tdata, client_data_t *client);

SendUninterested (torrent_data_t *tdata, client_data_t *client)
    => (torrent_data_t *tdata, client_data_t *client);
    
Choke (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);
    
Cancel (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);

Interested (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);

Uninterested (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);

Bitfield (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);

Unchoke (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (torrent_data_t *tdata, client_data_t *client);

SendRequest (torrent_data_t *tdata, client_data_t *client)
    => (client_data_t *client);

Have (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (torrent_data_t *tdata, client_data_t *client);

Piece (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (torrent_data_t *tdata, client_data_t *client, int piece);

Request (torrent_data_t *tdata, client_data_t *client, int type, int length, char *payload)
    => (client_data_t *client);
    
SendKeepAlives (torrent_data_t *tdata)
    => ();

GetClients () 
    => (int maxfd, fd_set *fds);

SelectSockets (int maxfd, fd_set *fds)
    => (fd_set *fds);

CheckSockets (fd_set *fds)
    => (torrent_data_t *tdata, client_data_t *client);

TrackerTimer () 
    => (torrent_data_t *tdata);

ChokeTimer ()
    => (torrent_data_t *tdata);

Connect ()
    => (torrent_data_t *tdata, int socket);

KeepAliveTimer ()
    => (torrent_data_t *tdata);

Listen ()
    => (torrent_data_t *tdata, client_data_t *client);

source TrackerTimer => CheckinWithTracker;
source ChokeTimer => UpdateChokeList;
source Connect => SetupConnection;
source Listen => Message;
source KeepAliveTimer => SendKeepAlives;

Listen = GetClients -> SelectSockets -> CheckSockets;
CheckinWithTracker = SendRequestToTracker -> GetTrackerResponse;
UpdateChokeList = PickChoked -> SendChokeUnchoke;
SetupConnection = Handshake -> SendBitfield;
Message = ReadMessage -> HandleMessage -> MessageDone;

CompletePiece:[_, _, piececomplete] = VerifyPiece -> SendHave -> SendUninterested;

HandleMessage:[_, _, choke, _, _] = Choke;
HandleMessage:[_, _, unchoke, _, _] = Unchoke -> SendRequest;
HandleMessage:[_, _, interested, _, _] = Interested;

HandleMessage:[_, _, uninterested, _, _] = Uninterested;
HandleMessage:[_, _, request, _, _] = Request;
HandleMessage:[_, _, cancel, _, _] = Cancel;
HandleMessage:[_, _, piece, _, _] = Piece -> CompletePiece -> SendRequest;
HandleMessage:[_, _, bitfield, _, _] = Bitfield;
HandleMessage:[_, _, have, _, _] = Have -> SendRequest;

atomic GetClients:{BigLock};
atomic CheckSockets:{BigLock};
atomic Message:{BigLock};
atomic CheckinWithTracker:{BigLock};
atomic UpdateChokeList:{BigLock};
atomic SetupConnection:{BigLock};
atomic SendKeepAlives:{BigLock};
