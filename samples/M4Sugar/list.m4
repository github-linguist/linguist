m4_define([m4_list_declare], [m4_do(
	[m4_define([$1_GET], [m4_expand([m4_list_nth([$1], $][1)])])],
	[m4_define([$1_FOREACH], [m4_foreach([item], [m4_dquote_elt(m4_list_contents([$1]))], m4_quote($][1))])],
)])

m4_define([m4_list_add], [m4_do(
	[m4_pushdef([_LIST_NAME], [[_LIST_$1]])],
	[m4_ifndef(_LIST_NAME,
		[m4_define(_LIST_NAME, m4_dquote(m4_escape([$2])))],
		[m4_define(_LIST_NAME, m4_dquote(m4_list_contents([$1]), m4_escape([$2])))],
	)],
	[m4_popdef([_LIST_NAME])],
)])

m4_define([m4_list_contents], [m4_do(
	[m4_pushdef([_LIST_NAME], [[_LIST_$1]])],
	[m4_ifndef(_LIST_NAME, [], m4_quote(_LIST_NAME))],
	[m4_popdef([_LIST_NAME])],
)])

m4_define([m4_list_nth], [m4_argn([$2], m4_list_contents([$1]))])

m4_define([m4_list_pop_front], [m4_do(
	[m4_pushdef([_LIST_NAME], [[_LIST_$1]])],
	[m4_car(m4_unquote(_LIST_NAME))],
	[m4_define(_LIST_NAME, m4_cdr(m4_unquote(_LIST_NAME)))],
	[m4_popdef([_LIST_NAME])],
)])

m4_define([m4_list_pop_back], [m4_do(
	[m4_pushdef([_LIST_NAME], [[_LIST_$1]])],
	[m4_define(_LIST_NAME, m4_dquote(m4_reverse(m4_unquote(_LIST_NAME))))],
	[m4_list_pop_front([$1])],
	[m4_define(_LIST_NAME, m4_dquote(m4_reverse(m4_unquote(_LIST_NAME))))],
	[m4_popdef([_LIST_NAME])],
)])

dnl
dnl $1: List name
dnl $2: What
dnl $3: If contains
dnl $4: If not
m4_define([m4_list_contains], [m4_do(
	[m4_foreach([item], m4_list_contents([$1]), m4_if(item, [$2], [[$3]], [[$4]]))]
)])

