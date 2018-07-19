-module(tree_traversal).
-export([main/0]).
-export([preorder/2, inorder/2, postorder/2, levelorder/2]).
-export([tnode/0, tnode/1, tnode/3]).

-define(NEWLINE, io:format("~n")).

tnode() -> {}.
tnode(V) -> {node, V, {}, {}}.
tnode(V,L,R) -> {node, V, L, R}.

preorder(_,{}) -> ok;
preorder(F,{node,V,L,R}) ->
    F(V), preorder(F,L), preorder(F,R).

inorder(_,{}) -> ok;
inorder(F,{node,V,L,R}) ->
    inorder(F,L), F(V), inorder(F,R).

postorder(_,{}) -> ok;
postorder(F,{node,V,L,R}) ->
    postorder(F,L), postorder(F,R), F(V).

levelorder(_, []) -> [];
levelorder(F, [{}|T]) -> levelorder(F, T);
levelorder(F, [{node,V,L,R}|T]) ->
    F(V), levelorder(F, T++[L,R]);
levelorder(F, X) -> levelorder(F, [X]).

main() ->
    Tree = tnode(1,
                 tnode(2,
                       tnode(4, tnode(7), tnode()),
                       tnode(5, tnode(), tnode())),
                 tnode(3,
                       tnode(6, tnode(8), tnode(9)),
                       tnode())),
    F = fun(X) -> io:format("~p ",[X]) end,
    preorder(F, Tree), ?NEWLINE,
    inorder(F, Tree), ?NEWLINE,
    postorder(F, Tree), ?NEWLINE,
    levelorder(F, Tree), ?NEWLINE.
