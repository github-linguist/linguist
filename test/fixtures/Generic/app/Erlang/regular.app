{application, regular, [
	{applications, [
		kernel,
		stdlib
	]},
	{description, "Regular application"},
	{mod, {regular_app, []}},
	{modules, [
		regular,
		regular_app,
		regular_server,
		regular_sup]},
	{registered, [regular_server]},
	{vsn, "0.1.0"}
]}.
