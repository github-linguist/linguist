-module(json).
-export([main/0]).

main() ->
	JSON =
		"{
		    \"firstName\": \"John\",
		    \"lastName\": \"Smith\",
		    \"age\": 25,
		    \"address\": {
		        \"streetAddress\": \"21 2nd Street\",
		        \"city\": \"New York\",
		        \"state\": \"NY\",
		        \"postalCode\": \"10021\"
		    },
		    \"phoneNumber\": [
		        {
		            \"type\": \"home\",
		            \"number\": \"212 555-1234\"
		        },
		        {
		            \"type\": \"fax\",
		            \"number\": \"646 555-4567\"
		        }
		    ]
		}",
	Erlang =
		{struct,
			[{"firstName","John"},
	         {"lastName","Smith"},
	         {"age",25},
	         {"address",
	          {struct,[{"streetAddress","21 2nd Street"},
	                   {"city","New York"},
	                   {"state","NY"},
	                   {"postalCode","10021"}]}},
	         {"phoneNumber",
	          {array,[{struct,[{"type","home"},{"number","212 555-1234"}]},
	                  {struct,[{"type","fax"},{"number","646 555-4567"}]}]}}]},
	io:format("JSON -> Erlang\n~p\n",[mochijson:decode(JSON)]),
	io:format("Erlang -> JSON\n~s\n",[mochijson:encode(Erlang)]).
