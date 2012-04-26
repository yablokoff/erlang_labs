-module(scalar).
-export([run/1, scalar_prod/2]).

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

gen_list(MaxValue, Size) ->
	do_n(fun random:uniform/1, MaxValue, Size).

scalar_prod(XArr, YArr) ->
	lists:zipwith(fun (X, Y) -> X*Y end, XArr, YArr).

run(F) ->
	XArr = gen_list(10, 10),
	YArr = gen_list(10, 10),
	io:format("List X: ~p~nList Y: ~p~nResult: ", [XArr, YArr]),
	F(XArr, YArr).