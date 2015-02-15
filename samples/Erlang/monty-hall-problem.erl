-module(monty_hall).

-export([main/0]).

main() ->
	random:seed(now()),
	{WinStay, WinSwitch} = experiment(100000, 0, 0),
	io:format("Switching wins ~p times.\n", [WinSwitch]),
	io:format("Staying wins ~p times.\n", [WinStay]).

experiment(0, WinStay, WinSwitch) ->
	{WinStay, WinSwitch};
experiment(N, WinStay, WinSwitch) ->
	Doors = setelement(random:uniform(3), {0,0,0}, 1),
	SelectedDoor = random:uniform(3),
	OpenDoor = open_door(Doors, SelectedDoor),
	experiment(
		N - 1,
		WinStay + element(SelectedDoor, Doors),
		WinSwitch + element(6 - (SelectedDoor + OpenDoor), Doors) ).

open_door(Doors,SelectedDoor) ->
	OpenDoor = random:uniform(3),
	case (element(OpenDoor, Doors) =:= 1) or (OpenDoor =:= SelectedDoor) of
		true -> open_door(Doors, SelectedDoor);
		false -> OpenDoor
	end.
