create or replace package plsqlguide is

-- Author  : Jared Petersen
-- Created : 9/22/2015 12:26:22 AM
-- Purpose : Basic PLSQL template/guide

/* Procedures */
procedure p_main;

end plsqlguide;
/
create or replace package body plsqlguide is

/* Main entry point (homepage) */
procedure p_main
	is
begin

htp.prn('
	<!DOCTYPE html>
		<html lang="en">
		<head>
			<meta charset="utf-8">
			<meta http-equiv="X-UA-Compatible" content="IE=edge">
			<meta name="viewport" content="width=device-width, initial-scale=1">
			<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
			<title>PL/SQL Sample Application</title>

			<!-- Bootstrap -->
			<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">

			<!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
			<!--[if lt IE 9]>
				<script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
				<script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
			<![endif]-->
		</head>
		<body>
			<!-- Static navbar -->
			<nav class="navbar navbar-default navbar-static-top">
				<div class="container">
					<div class="navbar-header">
						<a class="navbar-brand" href="#">PL/SQL Sample Application</a>
					</div>
				</div>
			</nav>

			<div class="container">
				<table class="table table-bordered">
					<tr>
						<th>#</th>
						<th>Name</th>
						<th>Description</th>
						<th>Quantity</th>
						<th>Price</th>
					</tr>
');

-- Fill out the parts table
for row in (select * from parts) loop
	htp.prn('
					<tr>
						<td>'||row.pid||'</td>
						<td>'||row.name||'</td>
						<td>'||row.description||'</td>
						<td>'||row.quantity||'</td>
						<td>'||row.price||'</td>
					</tr>
	');
end loop;

htp.prn('
    		</table>
			</div> <!-- /container -->

			<!-- jQuery (necessary for Bootstrap''s JavaScript plugins) -->
			<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
			<!-- Include all compiled plugins (below), or include individual files as needed -->
			<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"></script>
		</body>
	</html>
');

end p_main;

begin
  -- Initialization
  null;
end plsqlguide;
/
