-module(bfs).
-export([run/0]).

%
% Breadth-width search
% Farmer, wolf, goat and cabbage are goin to be moved to east-coast from west-coast
%

opposite(Side) ->
	if Side == e -> w;
	true -> e end.

move_farmer({F,W,G,C}) ->
	{opposite(F),W,G,C}.

move_wolf({F,W,G,C}) when F == W ->
	{opposite(F),opposite(W),G,C};
move_wolf(_) ->
	false.

move_goat({F,W,G,C}) when F == G ->
	{opposite(F),W,opposite(G),C};
move_goat(_) ->
	false.

move_cabbage({F,W,G,C}) when F == C ->
	{opposite(F),W,G,opposite(C)};
move_cabbage(_) ->
	false.

illegal_or_unsafe({F,X,X,_}) when F =/= X ->
	true;
illegal_or_unsafe({F,_,X,X}) when F =/= X ->
	true;
illegal_or_unsafe(false) ->
	true;
illegal_or_unsafe({_,_,_,_}) ->
	false.

make_new_state({F, LastPos, RestPath}) ->
	NewState = F(LastPos),
	Ill = illegal_or_unsafe(NewState),
	Mem = lists:member(NewState, RestPath),
	if not Ill, not Mem ->
		lists:append([[NewState],[LastPos],RestPath]);
		true -> false
	end.

bfs( [[{e,e,e,e}|RestPath]|_] ) ->
	lists:reverse([{e,e,e,e}|RestPath]);
bfs( [[LastPos|RestPath] | RestPaths] ) ->
	MovesAndPos = [
					{fun move_farmer/1, LastPos, RestPath},
					{fun move_wolf/1, LastPos, RestPath},
					{fun move_goat/1, LastPos, RestPath},
					{fun move_cabbage/1, LastPos, RestPath}
					],
	PossibleNewStates = lists:map(fun make_new_state/1, MovesAndPos),
	NewStates = lists:filter(fun (X) -> X =/= false end, PossibleNewStates),
	bfs(lists:append(RestPaths, NewStates)).

run() ->
	bfs([[{w,w,w,w}]]).