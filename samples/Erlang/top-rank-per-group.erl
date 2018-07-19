<-module( top_rank_per_group  ).

-export( [employees/0, employees_in_department/2, highest_payed/2, task/1] ).

-record( employee, {name, id, salery, department} ).

employees() ->
	[#employee{name="Tyler Bennett", id="E10297", salery=32000, department="D101"},
		#employee{name="John Rappl", id="E21437", salery=47000, department="D101"},
		#employee{name="George Woltman", id="E00127", salery=53500, department="D050"},
		#employee{name="Adam Smith", id="E63535", salery=18000, department="D202"},
		#employee{name="Claire Buckman", id="E39876", salery=27800, department="D202"},
		#employee{name="David McClellan", id="E04242", salery=41500, department="D101"},
		#employee{name="Rich Holcomb", id="E01234", salery=49500, department="D202"},
		#employee{name="Nathan Adams", id="E41298", salery=21900, department="D050"},
		#employee{name="Richard Potter", id="E43128", salery=15900, department="D101"},
		#employee{name="David Motsinger", id="E27002", salery=19250, department="D202"},
		#employee{name="Tim Sampair", id="E03033", salery=27000, department="D101"},
		#employee{name="Kim Arlich", id="E10001", salery=57000, department="D190"},
		#employee{name="Timothy Grove", id="E16398", salery=29900, department="D190"}].

employees_in_department( Department, Employees ) -> [X || #employee{department=D}=X <- Employees, D =:= Department].

highest_payed( N, Employees ) ->
	{Highest, _T} = lists:split( N, lists:reverse(lists:keysort(#employee.salery, Employees)) ),
	Highest.

task( N ) ->
	Employees = employees(),
	Departments = lists:usort( [X || #employee{department=X} <- Employees] ),
	Employees_in_departments = [employees_in_department(X, Employees) || X <- Departments],
	Highest_payed_in_departments = [highest_payed(N, Xs) || Xs <- Employees_in_departments],
	[task_write(X) || X <- Highest_payed_in_departments].



task_write( Highest_payeds ) ->
	[io:fwrite( "~p ~p: ~p~n", [Department, Salery, Name]) || #employee{department=Department, salery=Salery, name=Name} <- Highest_payeds],
	io:nl().
