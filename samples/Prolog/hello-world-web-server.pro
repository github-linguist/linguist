% The following modules are used in the main module to start a server.
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% The following module is used in every module that describes a page.
:- use_module(library(http/html_write)).

% Main entry point: starts the server on port 8080.
server :- http_server(http_dispatch, [port(8080)]).

% Defines the handler for the root URI /.
:- http_handler('/', say_goodbye, []).

% Defines the actual page content.
% In this case we're returning a page with the title "Howdy" and the content,
% wrapped in <h1></h1> tags, "Goodbye, World!".
say_goodbye(_Request) :- reply_html_page([title('Howdy')],
                                   [h1('Goodbye, World!')]).
