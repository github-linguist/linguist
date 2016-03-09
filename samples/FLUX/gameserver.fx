typedef engine isEngineMessage;
typedef turn isTurnMessage;
typedef connect isConnectMessage;
typedef disconnect isDisconnectMessage;

ClientMessage(char* data) => ();
ParseMessage(char* data) => (int type, int client, char* data);
ReadMessage(int type, int client, char* data) => ();

ParseEngine(int type, int client, char* data) => (int client, int direction);
DoEngine(int client, int direction) => ();

ParseTurn(int type, int client, char* data) => (int client, int direction);
DoTurn(int client, int direction) => ();

ParseConnect(int type, int client, char* data) 
	=> (int client, char* host, int port);
DoConnect(int client, char* host, int port) => ();

ParseDisconnect(int type, int client, char* data) => (int client);
DoDisconnect(int client) => ();

UpdateBoard(ClientList clients) => (ClientList clients);
SendData(ClientList clients) => ();

DoUpdate(ClientList clients) => ();

DataTimer() => (ClientList clients);

GetClients() => (ClientList clients);

Wait(ClientList clients) => (ClientList clients);

Listen () => (char* data);

source Listen => ClientMessage;
source DataTimer => DoUpdate;

DataTimer = GetClients -> Wait;

DoUpdate = UpdateBoard -> SendData;

ClientMessage=ParseMessage -> ReadMessage;

ReadMessage:[engine, _, _] = ParseEngine -> DoEngine;
ReadMessage:[turn, _, _] = ParseTurn -> DoTurn;
ReadMessage:[connect, _, _] = ParseConnect -> DoConnect;
ReadMessage:[disconnect, _, _] = ParseDisconnect -> DoDisconnect;

atomic GetClients:{client_lock};
atomic DoConnect:{client_lock};
atomic DoDisconnect:{client_lock};


