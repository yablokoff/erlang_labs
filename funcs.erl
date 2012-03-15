-module(funcs).
-export([quadeq/3, euclid/2]).

pow(Num) -> Num*Num.

%
% Quadratic equation
%

quadeq(A,B,C) when A =/= 0 -> 
	D = pow(B)-4*A*C,
	if D == 0 ->
		X1 = -B/(2*A),
		io:format("X1 = ~f;~n",[X1]);
	D < 0 ->
		io:format("No roots!~n");
	D > 0 ->
		X1 = (-B+math:sqrt(D))/(2*A),
		X2 = (-B-math:sqrt(D))/(2*A),
		io:format("X1 = ~f, X2 = ~f;~n",[X1,X2])
	end;
quadeq(_,_,_) -> io:format("This is not quadeq!").

%
% Euclid algorithm
%

euclid(Dividend, 0) ->
	Dividend;
euclid(Dividend, Divider) ->
	euclid(Divider, Dividend rem Divider).
