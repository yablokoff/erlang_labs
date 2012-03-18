-module(high_order).
-export([map/2, list_heads/1, gen_list_of_lists/3, run/3]).

%
% Generate random 2D-array with specified XSize and YSize
% and compound list of rows' heads.
%

do_n(F, X, N) ->
	do_n_ar(F, X, N, []).

do_n_ar(_, _, 0, List) ->
	List;
do_n_ar(F, X, N, List) ->
	do_n_ar(F, X, N-1, [F(X)|List]).

gen_list_of_lists(MaxValue, XSize, YSize) ->
	GenRow = fun(_) ->
		do_n(fun random:uniform/1, MaxValue, XSize) end,
	do_n(GenRow, true, YSize).

map(_,[]) ->
	[];
map(F, [H|Tail]) ->
	NewAcc = map(F, Tail),
	[F(H)|NewAcc].

list_heads(List) ->
	map(fun([H|_])->H end, List).

run(MaxValue, XSize, YSize) ->
	List = gen_list_of_lists(MaxValue, XSize, YSize),
	io:format("~p~n", [List]),
	list_heads(List).