-module(sem_graph).
-export([findLinks/0]).

%
% Semantic network
% Finds all relations with specified object, subject and type of relationship
%

map(_, _, []) ->
	[];
map(F, X, [H|Tail]) ->
	NewList = map(F, X, Tail),
	[F(H,X)|NewList].

findFacts(Obj, Db) ->
	Relations = [ {Link, Subject} || {Object, Link, Subject} <- Db, Object == Obj ],
	InclusionRelations = [ Subject || {Link, Subject} <- Relations, Link == is ],
	NewRelations = map(fun findFacts/2, Db, InclusionRelations),
	lists:flatten(lists:merge(Relations, NewRelations)).

findLinks()
	->
	Db = [{avtoVAZ, is, auto_producer}, {priora, produced_by, avtoVAZ}, {priora, presented_in, 2007},
			{priora, is, passenger_car}, {kalina, produced_by, avtoVAZ}, {kalina, presented_in, 2004},
			{kalina, is, passenger_car}, {passenger_car, has_max_engine_volume, three_l}, {passenger_car, has_max_price, 25000},
			{best_selling_car_of_2011, is, priora}, {passenger_car, is, vehicle}, {avtoVAZ, presented_in, 1969},
			{the_biggest_steel_consumer, is, avtoVAZ}
		 ],
	{ok,Obj} = io:read("Set object: "),
	AllFacts = findFacts(Obj,Db),
	Subjs = [{Object, Link} || {Object, Link, Subject} <- Db, Subject == Obj],
	{ok,L} = io:read("Set link: "),
	Links = [{Object, Link, Subject} || {Object, Link, Subject} <- Db, Link == L],
	if AllFacts =/= [] ->
		io:fwrite("Facts with object : ~W~n", [AllFacts,19]);
		true -> empty
	end,
	if Subjs =/= [] ->
		io:fwrite("Facts with subject : ~W~n", [Subjs,19]);
		true -> empty
	end,
	if Links =/= [] ->
		io:fwrite("Facts with link : ~W~n", [Links,19]);
		true -> empty
	end.