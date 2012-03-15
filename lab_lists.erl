-module(lab_lists).
-export([lilen/1, lilen_tail/1, substitute/3]).

% normal recursion
lilen([]) -> 0;
lilen([_|Tail]) -> 1 + lilen(Tail).

% tail recursion
lilen_tail(List) -> lilen_acc(List,0).
lilen_acc([],Acc) -> Acc;
lilen_acc([_|Tail],Acc) -> lilen_acc(Tail,Acc+1).

substitute([], What, To) -> [];
substitute([Elem|Tail], What, To) ->
	Mod_tail = substitute(Tail, What, To),
	if Elem =:= What -> [To | Mod_tail];
		Elem =/= What -> [Elem | Mod_tail]
	end.


