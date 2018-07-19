-module(chat).

-export([start/0, start/1]).

-record(client, {name=none, socket=none}).

start() -> start(8080).
start(Port) ->
    register(server, spawn(fun() -> server() end)),
    {ok, LSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    accept(LSocket).

% main loop for message dispatcher
server() -> server([]).
server(Clients) ->
    receive
        {join, Client=#client{name = Name, socket = Socket}} ->
            self() ! {say, Socket, "has joined." ++ [10, 13]},
            server(Clients ++ [Client]);
        {leave, Socket} ->
            {value, #client{name = Name}, List} = lists:keytake(Socket, 3, Clients),
            self() ! {say, none, Message = "has left."},
            server(List);
        {say, Socket, Data} ->
            {value, #client{name = From}, List} = lists:keytake(Socket, 3, Clients),
            Message = From ++ " : " ++ Data,
            lists:map(fun(#client{socket = S}) ->
                    gen_tcp:send(S, Message)
                end, List)
    end,
    server(Clients).

% accepts connections then spawns the client handler
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> connecting(Socket) end),
    accept(LSocket).

% when client is first connect send prompt for user name
connecting(Socket) ->
    gen_tcp:send(Socket, "What is your name? "),
    case listen(Socket) of
        {ok, N} ->
            Name = binary_to_list(N),
            server ! {join, #client{name =  lists:sublist(Name, 1, length(Name) - 2), socket = Socket} },
            client(Socket);
        _ -> ok
    end.

% main client loop that listens for data
client(Socket) ->
    case listen(Socket) of
        {ok, Data} ->
            server ! {say, Socket, binary_to_list(Data)},
            client(Socket);
        _ -> server ! {leave, Socket}
    end.

% utility function that listens for data on a socket
listen(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        Response -> Response
    end.
