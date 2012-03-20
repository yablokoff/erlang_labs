-module(high_order).
-export([gen_list_of_lists/3, run/4, first_of/1, second_of/1, third_of/1]).

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

first_of(List) ->
	lists:nth(1,List).

second_of(List) ->
	lists:nth(2,List).

third_of(List) ->
	lists:nth(3,List).

list_heads(List, Fun) ->
	map(Fun, List).

run(MaxValue, XSize, YSize, Fun) ->
	List = gen_list_of_lists(MaxValue, XSize, YSize),
	io:format("~p~n", [List]),
	list_heads(List, Fun).