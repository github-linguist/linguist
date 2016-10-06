/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004-2010, University of Amsterdam,
			      VU University Amsterdam

    This program is free software; you can redistribute it and/o<r
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cpa_admin,
	  [ change_password_form//1
	  ]).
:- use_module(user(user_db)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http_settings)).

/** <module> ClioPatria administrative interface

This module provides HTTP services to perform administrative actions.

@tbd	Ideally, this module should be split into an api-part, a
	component-part and the actual pages.  This also implies that
	the current `action'-operations must (optionally) return
	machine-friendly results.
*/


:- http_handler(cliopatria('admin/listUsers'),		   list_users,		    []).
:- http_handler(cliopatria('admin/form/createAdmin'),	   create_admin,	    []).
:- http_handler(cliopatria('admin/form/addUser'),	   add_user_form,	    []).
:- http_handler(cliopatria('admin/form/addOpenIDServer'),  add_openid_server_form,  []).
:- http_handler(cliopatria('admin/addUser'),		   add_user,		    []).
:- http_handler(cliopatria('admin/selfRegister'),	   self_register,	    []).
:- http_handler(cliopatria('admin/addOpenIDServer'),	   add_openid_server,	    []).
:- http_handler(cliopatria('admin/form/editUser'),	   edit_user_form,	    []).
:- http_handler(cliopatria('admin/editUser'),		   edit_user,		    []).
:- http_handler(cliopatria('admin/delUser'),		   del_user,		    []).
:- http_handler(cliopatria('admin/form/editOpenIDServer'), edit_openid_server_form, []).
:- http_handler(cliopatria('admin/editOpenIDServer'),	   edit_openid_server,	    []).
:- http_handler(cliopatria('admin/delOpenIDServer'),	   del_openid_server,	    []).
:- http_handler(cliopatria('admin/form/changePassword'),   change_password_form,    []).
:- http_handler(cliopatria('admin/changePassword'),	   change_password,	    []).
:- http_handler(cliopatria('user/form/login'),		   login_form,		    []).
:- http_handler(cliopatria('user/login'),		   user_login,		    []).
:- http_handler(cliopatria('user/logout'),		   user_logout,		    []).
:- http_handler(cliopatria('admin/settings'),		   settings,		    []).
:- http_handler(cliopatria('admin/save_settings'),	   save_settings,	    []).

%%	list_users(+Request)
%
%	HTTP Handler listing registered users.

list_users(_Request) :-
	authorized(admin(list_users)),
	if_allowed(admin(user(edit)),   [edit(true)], UserOptions),
	if_allowed(admin(openid(edit)), [edit(true)], OpenIDOptions),
	reply_html_page(cliopatria(default),
			title('Users'),
			[ h1('Users'),
			  \user_table(UserOptions),
			  p(\action(location_by_id(add_user_form), 'Add user')),
			  h1('OpenID servers'),
			  \openid_server_table(OpenIDOptions),
			  p(\action(location_by_id(add_openid_server_form), 'Add OpenID server'))
			]).

if_allowed(Token, Options, Options) :-
	logged_on(User, anonymous),
	catch(check_permission(User, Token), _, fail), !.
if_allowed(_, _, []).

%%	user_table(+Options)//
%
%	HTML component generating a table of registered users.

user_table(Options) -->
	{ setof(U, current_user(U), Users)
	},
	html([ table([ class(block)
		     ],
		     [ tr([ th('UserID'),
			    th('RealName'),
			    th('On since'),
			    th('Idle')
			  ])
		     | \list_users(Users, Options)
		     ])
	     ]).

list_users([], _) -->
	[].
list_users([User|T], Options) -->
	{ user_property(User, realname(Name)),
	  findall(Idle-Login,
		  user_property(User, connection(Login, Idle)),
		  Pairs0),
	  keysort(Pairs0, Pairs),
	  (   Pairs == []
	  ->  OnLine = (-)
	  ;   length(Pairs, N),
	      Pairs = [Idle-Login|_],
	      OnLine = online(Login, Idle, N)
	  )
	},
	html(tr([ td(User),
		  td(Name),
		  td(\on_since(OnLine)),
		  td(\idle(OnLine)),
		  \edit_user_button(User, Options)
		])),
	list_users(T, Options).

edit_user_button(User, Options) -->
	{ option(edit(true), Options) }, !,
	html(td(a(href(location_by_id(edit_user_form)+'?user='+encode(User)), 'Edit'))).
edit_user_button(_, _) -->
	[].

on_since(online(Login, _Idle, _Connections)) --> !,
	{ format_time(string(Date), '%+', Login)
	},
	html(Date).
on_since(_) -->
	html(-).

idle(online(_Login, Idle, _Connections)) -->
	{ mmss_duration(Idle, String)
	},
	html(String).
idle(_) -->
	html(-).


mmss_duration(Time, String) :-		% Time in seconds
	Secs is round(Time),
	Hour is Secs // 3600,
	Min  is (Secs // 60) mod 60,
	Sec  is Secs mod 60,
	format(string(String), '~`0t~d~2|:~`0t~d~5|:~`0t~d~8|', [Hour, Min, Sec]).



		 /*******************************
		 *	      ADD USERS		*
		 *******************************/

%%	create_admin(+Request)
%
%	Create the administrator login.

create_admin(_Request) :-
	(   current_user(_)
	->  throw(error(permission_error(create, user, admin),
			context(_, 'Already initialized')))
	;   true
	),
	reply_html_page(cliopatria(default),
			title('Create administrator'),
			[ h1(align(center), 'Create administrator'),

			  p('No accounts are available on this server. \c
			  This form allows for creation of an administrative \c
			  account that can subsequently be used to create \c
			  new users.'),

			  \new_user_form([ user(admin),
					   real_name('Administrator')
					 ])
			]).


%%	add_user_form(+Request)
%
%	Form to register a user.

add_user_form(_Request) :-
	authorized(admin(add_user)),
	reply_html_page(cliopatria(default),
			title('Add new user'),
			[ \new_user_form([])
			]).

new_user_form(Options) -->
	{ (   option(user(User), Options)
	  ->  UserOptions = [value(User)],
	      PermUser = User
	  ;   UserOptions = [],
	      PermUser = (-)
	  )
	},
	html([ h1('Add new user'),
	       form([ action(location_by_id(add_user)),
		      method('POST')
		    ],
		    table([ class((form))
			  ],
			  [ \realname(Options),
			    \input(user,     'Login',
				   UserOptions),
			    \input(pwd1,     'Password',
				   [type(password)]),
			    \input(pwd2,     'Retype',
				   [type(password)]),
			    \permissions(PermUser),
			    tr(class(buttons),
			       td([ colspan(2),
				    align(right)
				  ],
				  input([ type(submit),
					  value('Create')
					])))
			  ]))
	     ]).


input(Name, Label, Options) -->
	html(tr([ th(align(right), Label),
		  td(input([name(Name),size(40)|Options]))
		])).

%	Only provide a realname field if this is not already given. This
%	is because firefox determines the login user from the text field
%	immediately above the password entry. Other   browsers may do it
%	different, so only having one text-field  is probably the savest
%	solution.

realname(Options) -->
	{ option(real_name(RealName), Options) }, !,
	hidden(realname, RealName).
realname(_Options) -->
	input(realname, 'Realname', []).


%%	add_user(+Request)
%
%	API  to  register  a  new  user.  The  current  user  must  have
%	administrative rights or the user-database must be empty.

add_user(Request) :-
	(   \+ current_user(_)
	->  FirstUser = true
	;   authorized(admin(add_user))
	),
	http_parameters(Request,
			[ user(User),
			  realname(RealName),
			  pwd1(Password),
			  pwd2(Retype),
			  read(Read),
			  write(Write),
			  admin(Admin)
			],
			[ attribute_declarations(attribute_decl)
			]),
	(   current_user(User)
	->  throw(error(permission_error(create, user, User),
			context(_, 'Already present')))
	;   true
	),
	(   Password == Retype
	->  true
	;   throw(password_mismatch)
	),
	password_hash(Password, Hash),
	phrase(allow(Read, Write, Admin), Allow),
	user_add(User,
		 [ realname(RealName),
		   password(Hash),
		   allow(Allow)
		 ]),
	(   FirstUser == true
	->  user_add(anonymous,
		     [ realname('Define rights for not-logged in users'),
		       allow([read(_,_)])
		     ]),
	    reply_login([user(User), password(Password)])
	;   list_users(Request)
	).

%%	self_register(Request)
%
%	Self-register and login a new user if
%	cliopatria:enable_self_register is set to true.
%       Users are registered with full read
%	and limited (annotate-only) write access.
%
%	Returns a HTTP 403 forbidden error if:
%	- cliopatria:enable_self_register is set to false
%	- the user already exists

self_register(Request) :-
	http_location_by_id(self_register, MyUrl),
	(   \+ setting(cliopatria:enable_self_register, true)
	->  throw(http_reply(forbidden(MyUrl)))
	;   true
	),
	http_parameters(Request,
			[ user(User),
			  realname(RealName),
			  password(Password)
			],
			[ attribute_declarations(attribute_decl)
			]),
	(   current_user(User)
	->  throw(http_reply(forbidden(MyUrl)))
	;   true
	),
	password_hash(Password, Hash),
	Allow = [ read(_,_), write(_,annotate) ],
	user_add(User, [realname(RealName), password(Hash), allow(Allow)]),
	reply_login([user(User), password(Password)]).


%%	edit_user_form(+Request)
%
%	Form to edit user properties

edit_user_form(Request) :-
	authorized(admin(user(edit))),
	http_parameters(Request,
			[ user(User)
			],
			[ attribute_declarations(attribute_decl)
			]),

	reply_html_page(cliopatria(default),
			title('Edit user'),
			\edit_user_form(User)).

%%	edit_user_form(+User)//
%
%	HTML component to edit the properties of User.

edit_user_form(User) -->
	{ user_property(User, realname(RealName))
	},
	html([ h1(['Edit user ', User, ' (', RealName, ')']),

	       form([ action(location_by_id(edit_user)),
		      method('POST')
		    ],
		    [ \hidden(user, User),
		      table([ class((form))
			    ],
			    [ \user_property(User, realname, 'Real name', []),
			      \permissions(User),
			      tr(class(buttons),
				 td([ colspan(2),
				      align(right)
				    ],
				    input([ type(submit),
					    value('Modify')
					  ])))
			    ])
		    ]),

	       p(\action(location_by_id(del_user)+'?user='+encode(User),
			 [ 'Delete user ', b(User), ' (', i(RealName), ')' ]))
	     ]).

user_property(User, Name, Label, Options) -->
	{  Term =.. [Name, Value],
	   user_property(User, Term)
	-> O2 = [value(Value)|Options]
	;  O2 = Options
	},
	html(tr([ th(class(p_name), Label),
		  td(input([name(Name),size(40)|O2]))
		])).

permissions(User) -->
	html(tr([ th(class(p_name), 'Permissions'),
		  td([ \permission_checkbox(User, read,  'Read'),
		       \permission_checkbox(User, write, 'Write'),
		       \permission_checkbox(User, admin, 'Admin')
		     ])
		])).

permission_checkbox(User, Name, Label) -->
	{ (   User \== (-),
	      (	  user_property(User, allow(Actions))
	      ->  true
	      ;	  openid_server_property(User, allow(Actions))
	      ),
	      pterm(Name, Action),
	      memberchk(Action, Actions)
	  ->  Opts = [checked]
	  ;   def_user_permissions(User, DefPermissions),
	      memberchk(Name, DefPermissions)
	  ->  Opts = [checked]
	  ;   Opts = []
	  )
	},
	html([ input([ type(checkbox),
		       name(Name)
		     | Opts
		     ]),
	       Label
	     ]).

def_user_permissions(-, [read]).
def_user_permissions(admin, [read, write, admin]).


%%	edit_user(Request)
%
%	Handle reply from edit user form.

edit_user(Request) :-
	authorized(admin(user(edit))),
	http_parameters(Request,
			[ user(User),
			  realname(RealName,
				   [ optional(true),
				     length > 2,
				     description('Comment on user identifier-name')
				   ]),
			  read(Read),
			  write(Write),
			  admin(Admin)
			],
			[ attribute_declarations(attribute_decl)
			]),
	modify_user(User, realname(RealName)),
	modify_permissions(User, Read, Write, Admin),
	list_users(Request).


modify_user(User, Property) :-
	Property =.. [_Name|Value],
	(   (   var(Value)
	    ;	Value == ''
	    )
	->  true
	;   set_user_property(User, Property)
	).

modify_permissions(User, Read, Write, Admin) :-
	phrase(allow(Read, Write, Admin), Allow),
	set_user_property(User, allow(Allow)).

allow(Read, Write, Admin) -->
	allow(read, Read),
	allow(write, Write),
	allow(admin, Admin).

allow(Access, on) -->
	{ pterm(Access, Allow)
	}, !,
	[ Allow
	].
allow(_Access, off) --> !,
	[].

pterm(read,  read(_Repositiory, _Action)).
pterm(write, write(_Repositiory, _Action)).
pterm(admin, admin(_Action)).


%%	del_user(+Request)
%
%	Delete a user

del_user(Request) :- !,
	authorized(admin(del_user)),
	http_parameters(Request,
			[ user(User)
			],
			[ attribute_declarations(attribute_decl)
			]),
	(   User == admin
	->  throw(error(permission_error(delete, user, User), _))
	;   true
	),
	user_del(User),
	list_users(Request).


%%	change_password_form(+Request)
%
%	Allow user to change the password

change_password_form(_Request) :-
	logged_on(User), !,
	user_property(User, realname(RealName)),
	reply_html_page(cliopatria(default),
			title('Change password'),
			[ h1(['Change password for ', User, ' (', RealName, ')']),

			  \change_password_form(User)
			]).
change_password_form(_Request) :-
	throw(error(context_error(not_logged_in), _)).


%%	change_password_form(+UserID)//
%
%	HTML component that shows a form   for changing the password for
%	UserID.

change_password_form(User) -->
	html(form([ action(location_by_id(change_password)),
		    method('POST')
		  ],
		  [ table([ id('change-password-form'),
			    class(form)
			  ],
			  [ \user_or_old(User),
			    \input(pwd1,     'New Password',
				   [type(password)]),
			    \input(pwd2,     'Retype',
				   [type(password)]),
			    tr(class(buttons),
			       td([ align(right),
				    colspan(2)
				  ],
				  input([ type(submit),
					  value('Change password')
					])))
			  ])
		  ])).

user_or_old(admin) --> !,
	input(user, 'User', []).
user_or_old(_) -->
	input(pwd0, 'Old password', [type(password)]).


%%	change_password(+Request)
%
%	HTTP handler to change the password. The user must be logged on.

change_password(Request) :-
	logged_on(Login), !,
	http_parameters(Request,
			[ user(User,     [ optional(true),
					   description('User identifier-name')
					 ]),
			  pwd0(Password, [ optional(true),
					   description('Current password')
					 ]),
			  pwd1(New),
			  pwd2(Retype)
			],
			[ attribute_declarations(attribute_decl)
			]),
	(   Login == admin
	->  (   current_user(User)
	    ->	true
	    ;	throw(error(existence_error(user, User), _))
	    )
	;   Login = User,
	    validate_password(User, Password)
	),
	(   New == Retype
	->  true
	;   throw(password_mismatch)
	),
	password_hash(New, Hash),
	set_user_property(User, password(Hash)),
	reply_html_page(cliopatria(default),
			'Password changed',
			[ h1(align(center), 'Password changed'),
			  p([ 'Your password has been changed successfully' ])
			]).
change_password(_Request) :-
	throw(error(context_error(not_logged_in), _)).



		 /*******************************
		 *	       LOGIN		*
		 *******************************/

%%	login_form(+Request)
%
%	HTTP handler that presents a form to login.

login_form(_Request) :-
	reply_html_page(cliopatria(default),
			'Login',
			[ h1(align(center), 'Login'),
			  form([ action(location_by_id(user_login)),
				 method('POST')
			       ],
			       table([ tr([ th(align(right), 'User:'),
					    td(input([ name(user),
						       size(40)
						     ]))
					  ]),
				       tr([ th(align(right), 'Password:'),
					    td(input([ type(password),
						       name(password),
						       size(40)
						     ]))
					  ]),
				       tr([ td([ align(right), colspan(2) ],
					       input([ type(submit),
						       value('Login')
						     ]))
					  ])
				     ])
			      )
			]).

%%	user_login(+Request)
%
%	Handle  =user=  and  =password=.  If    there   is  a  parameter
%	=return_to= or =|openid.return_to|=, reply using   a redirect to
%	the given URL. Otherwise display a welcome page.

user_login(Request) :- !,
	http_parameters(Request,
			[ user(User),
			  password(Password),
			  'openid.return_to'(ReturnTo, [optional(true)]),
			  'return_to'(ReturnTo, [optional(true)])
			],
			[ attribute_declarations(attribute_decl)
			]),
	(   var(ReturnTo)
	->  Extra = []
	;   Extra = [ return_to(ReturnTo) ]
	),
	reply_login([ user(User),
		      password(Password)
		    | Extra
		    ]).


reply_login(Options) :-
	option(user(User), Options),
	option(password(Password), Options),
	validate_password(User, Password), !,
	login(User),
	(   option(return_to(ReturnTo), Options)
	->  throw(http_reply(moved_temporary(ReturnTo)))
	;   reply_html_page(cliopatria(default),
			    title('Login ok'),
			    h1(align(center), ['Welcome ', User]))
	).
reply_login(_) :-
	reply_html_page(cliopatria(default),
			title('Login failed'),
			[ h1('Login failed'),
			  p(['Password incorrect'])
			]).

%%	user_logout(+Request)
%
%	Logout the current user

user_logout(_Request) :-
	logged_on(User), !,
	logout(User),
	reply_html_page(cliopatria(default),
			title('Logout'),
			h1(align(center), ['Logged out ', User])).
user_logout(_Request) :-
	reply_html_page(cliopatria(default),
			title('Logout'),
			[ h1(align(center), ['Not logged on']),
			  p(['Possibly you are logged out because the session ',
			     'has timed out.'])
			]).

%%	attribute_decl(+Param, -DeclObtions) is semidet.
%
%	Provide   reusable   parameter   declarations   for   calls   to
%	http_parameters/3.

attribute_decl(user,
	       [ description('User identifier-name'),
		 length > 1
	       ]).
attribute_decl(realname,
	       [ description('Comment on user identifier-name')
	       ]).
attribute_decl(description,
	       [ optional(true),
		 description('Descriptive text')
	       ]).
attribute_decl(password,
	       [ description('Password')
	       ]).
attribute_decl(pwd1,
	       [ length > 5,
		 description('Password')
	       ]).
attribute_decl(pwd2,
	       [ length > 5,
		 description('Re-typed password')
	       ]).
attribute_decl(openid_server,
	       [ description('URL of an OpenID server')
	       ]).
attribute_decl(read,
	       [ description('Provide read-only access to the RDF store')
	       | Options])   :- bool(off, Options).
attribute_decl(write,
	       [ description('Provide write access to the RDF store')
	       | Options])   :- bool(off, Options).
attribute_decl(admin,
	       [ description('Provide administrative rights')
	       | Options])   :- bool(off, Options).

bool(Def,
     [ default(Def),
       oneof([on, off])
     ]).


		 /*******************************
		 *	    OPENID ADMIN	*
		 *******************************/

%%	add_openid_server_form(+Request)
%
%	Return an HTML page to add a new OpenID server.

add_openid_server_form(_Request) :-
	authorized(admin(add_openid_server)),
	reply_html_page(cliopatria(default),
			title('Add OpenID server'),
			[ \new_openid_form
			]).


%%	new_openid_form// is det.
%
%	Present form to add a new OpenID provider.

new_openid_form -->
	html([ h1('Add new OpenID server'),
	       form([ action(location_by_id(add_openid_server)),
		      method('GET')
		    ],
		    table([ id('add-openid-server'),
			    class(form)
			  ],
			  [ \input(openid_server, 'Server homepage', []),
			    \input(openid_description, 'Server description',
				   []),
			    \permissions(-),
			    tr(class(buttons),
			       td([ colspan(2),
				    align(right)
				  ],
				  input([ type(submit),
					  value('Create')
					])))
			  ])),
	       p([ 'Use this form to define access rights for users of an ',
		   a(href('http://www.openid.net'), 'OpenID'), ' server. ',
		   'The special server ', code(*), ' specifies access for all OpenID servers. ',
		   'Here are some examples of servers:'
		 ]),
	       ul([ li(code('http://myopenid.com'))
		  ])
	     ]).


%%	add_openid_server(+Request)
%
%	Allow access from an OpenID server

add_openid_server(Request) :-
	authorized(admin(add_openid_server)),
	http_parameters(Request,
			[ openid_server(Server0,
					[ description('URL of the server to allow')]),
			  openid_description(Description,
					     [ optional(true),
					       description('Description of the server')
					     ]),
			  read(Read),
			  write(Write)
			],
			[ attribute_declarations(attribute_decl)
			]),
	phrase(allow(Read, Write, off), Allow),
	canonical_url(Server0, Server),
	Options = [ description(Description),
		    allow(Allow)
		  ],
	remove_optional(Options, Properties),
	openid_add_server(Server, Properties),
	list_users(Request).

remove_optional([], []).
remove_optional([H|T0], [H|T]) :-
	arg(1, H, A),
	nonvar(A), !,
	remove_optional(T0, T).
remove_optional([_|T0], T) :-
	remove_optional(T0, T).


canonical_url(Var, Var) :-
	var(Var), !.
canonical_url(*, *) :- !.
canonical_url(URL0, URL) :-
	parse_url(URL0, Parts),
	parse_url(URL, Parts).


%%	edit_openid_server_form(+Request)
%
%	Form to edit user properties

edit_openid_server_form(Request) :-
	authorized(admin(openid(edit))),
	http_parameters(Request,
			[ openid_server(Server)
			],
			[ attribute_declarations(attribute_decl)
			]),

	reply_html_page(cliopatria(default),
			title('Edit OpenID server'),
			\edit_openid_server_form(Server)).

edit_openid_server_form(Server) -->
	html([ h1(['Edit OpenID server ', Server]),

	       form([ action(location_by_id(edit_openid_server)),
		      method('GET')
		    ],
		    [ \hidden(openid_server, Server),
		      table([ class(form)
			    ],
			    [ \openid_property(Server, description, 'Description', []),
			      \permissions(Server),
			      tr(class(buttons),
				 td([ colspan(2),
				      align(right)
				    ],
				    input([ type(submit),
					    value('Modify')
					  ])))
			    ])
		    ]),

	       p(\action(location_by_id(del_openid_server) +
			 '?openid_server=' + encode(Server),
			 [ 'Delete ', b(Server) ]))
	     ]).


openid_property(Server, Name, Label, Options) -->
	{  Term =.. [Name, Value],
	   openid_server_property(Server, Term)
	-> O2 = [value(Value)|Options]
	;  O2 = Options
	},
	html(tr([ th(align(right), Label),
		  td(input([name(Name),size(40)|O2]))
		])).


%%	openid_server_table(+Options)//
%
%	List registered openid servers

openid_server_table(Options) -->
	{ setof(S, openid_current_server(S), Servers), !
	},
	html([ table([ class(block)
		     ],
		     [ tr([ th('Server'),
			    th('Description')
			  ])
		     | \openid_list_servers(Servers, Options)
		     ])
	     ]).
openid_server_table(_) -->
	[].

openid_list_servers([], _) -->
	[].
openid_list_servers([H|T], Options) -->
	openid_list_server(H, Options),
	openid_list_servers(T, Options).

openid_list_server(Server, Options) -->
	html(tr([td(\openid_server(Server)),
		 td(\openid_field(Server, description)),
		 \edit_openid_button(Server, Options)
		])).

edit_openid_button(Server, Options) -->
	{ option(edit(true), Options) }, !,
	html(td(a(href(location_by_id(edit_openid_server_form) +
		       '?openid_server='+encode(Server)
		      ), 'Edit'))).
edit_openid_button(_, _) --> [].



openid_server(*) --> !,
	html(*).
openid_server(Server) -->
	html(a(href(Server), Server)).

openid_field(Server, Field) -->
	{ Term =.. [Field, Value],
	  openid_server_property(Server, Term)
	}, !,
	html(Value).
openid_field(_, _) -->
	[].


%%	edit_openid_server(Request)
%
%	Handle reply from OpenID server form.

edit_openid_server(Request) :-
	authorized(admin(openid(edit))),
	http_parameters(Request,
			[ openid_server(Server),
			  description(Description),
			  read(Read),
			  write(Write),
			  admin(Admin)
			],
			[ attribute_declarations(attribute_decl)
			]),
	modify_openid(Server, description(Description)),
	openid_modify_permissions(Server, Read, Write, Admin),
	list_users(Request).


modify_openid(User, Property) :-
	Property =.. [_Name|Value],
	(   (   var(Value)
	    ;	Value == ''
	    )
	->  true
	;   openid_set_property(User, Property)
	).


openid_modify_permissions(Server, Read, Write, Admin) :-
	phrase(allow(Read, Write, Admin), Allow),
	openid_set_property(Server, allow(Allow)).


%%	del_openid_server(+Request)
%
%	Delete an OpenID Server

del_openid_server(Request) :- !,
	authorized(admin(openid(delete))),
	http_parameters(Request,
			[ openid_server(Server)
			],
			[ attribute_declarations(attribute_decl)
			]),
	openid_del_server(Server),
	list_users(Request).


		 /*******************************
		 *	       SETTINGS		*
		 *******************************/

%%	settings(+Request)
%
%	Show current settings. If user  has administrative rights, allow
%	editing the settings.

settings(_Request) :-
	(   catch(authorized(admin(edit_settings)), _, fail)
	->  Edit = true
	;   authorized(read(admin, settings)),
	    Edit = false
	),
	reply_html_page(cliopatria(default),
			title('Settings'),
			[ h1('Application settings'),
			  \http_show_settings([ edit(Edit),
						hide_module(false),
						action('save_settings')
					      ]),
			  \warn_no_edit(Edit)
			]).

warn_no_edit(true) --> !.
warn_no_edit(_) -->
	html(p(id(settings_no_edit),
	       [ a(href(location_by_id(login_form)), 'Login'),
		 ' as ', code(admin), ' to edit the settings.' ])).

%%	save_settings(+Request)
%
%	Save modified settings.

save_settings(Request) :-
	authorized(admin(edit_settings)),
	reply_html_page(cliopatria(default),
			title('Save settings'),
			\http_apply_settings(Request, [save(true)])).


		 /*******************************
		 *		EMIT		*
		 *******************************/

%%	hidden(+Name, +Value)
%
%	Create a hidden input field with given name and value

hidden(Name, Value) -->
	html(input([ type(hidden),
		     name(Name),
		     value(Value)
		   ])).

action(URL, Label) -->
	html([a([href(URL)], Label), br([])]).
