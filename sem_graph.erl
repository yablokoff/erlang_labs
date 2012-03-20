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

findParents(Obj, Db) ->
	%
	% Make list of "is-parents" for Obj
	%
	Relations = [ Subject || {Object, Link, Subject} <- Db, Object == Obj, Link == is ],
	NewRelations = map(fun findParents/2, Db, Relations),
	lists:flatten(lists:merge(Relations, NewRelations)).
	
findChildren(Subj, Db) ->
	%
	% Make list of "is-children" for Subj
	%
	Relations = [ Object || {Object, Link, Subject} <- Db, Subject == Subj, Link == is ],
	NewRelations = map(fun findChildren/2, Db, Relations),
	lists:flatten(lists:merge(Relations, NewRelations)).

findChildrenAndMe(Subj, Db) ->
	%
	% Make list of "is-children" and Subj for Subj
	%
	Children = findChildren(Subj, Db),
	[Subj|Children].

findSpecs(Obj, Db) ->
	%
	% Find all first-level specs for Obj
	%
	[ {Object, Link, Subject} || {Object, Link, Subject} <- Db, Object == Obj ].

topInIsHierarchy({ {_,Subj}, Db }) ->
	%
	% Check whether Subj is the top-element of is-hierarchy
	%
	Parents = findParents(Subj, Db),
	if Parents =:= [] ->
		true;
		true -> false
	end.

findLinks()	->
	Db = [{avtoVAZ, is, auto_producer}, {priora, produced_by, avtoVAZ}, {priora, presented_in, 2007},
			{priora, is, passenger_car}, {kalina, produced_by, avtoVAZ}, {kalina, presented_in, 2004},
			{kalina, is, passenger_car}, {passenger_car, has_max_engine_volume, three_l}, {passenger_car, has_max_price, 25000},
			{a157ok, is, priora}, {passenger_car, is, vehicle}, {avtoVAZ, presented_in, 1969},
			{the_biggest_steel_consumer, is, avtoVAZ}, {vehicle, has, wheels}
		 ],
	{ok,Obj} = io:read("Set object: "),	
	Parents = findParents(Obj,Db),
	ParentsFacts = lists:flatten(map(fun findSpecs/2, Db, Parents)),
	Children = findChildrenAndMe(Obj,Db),
	ChildrenFacts = lists:flatten(map(fun findSpecs/2, Db, Children)),
	
	{ok,L} = io:read("Set link: "),
	DirtyLinks = [ {Object, Subject} || {Object, Link, Subject} <- Db, Link == L],
	if L =:= is ->
			LinksWithDb = lists:filter(
						fun topInIsHierarchy/1, 
						lists:zip(DirtyLinks,lists:duplicate(length(DirtyLinks), Db))
					),
			{Links,_} = lists:unzip(LinksWithDb);
		true -> 
			Links = DirtyLinks
	end,
	{LinksObjs, LinksSubjs} = lists:unzip(Links),
	AllLinks = lists:zip3(
					map(fun findChildrenAndMe/2, Db, LinksObjs),
					lists:duplicate(length(LinksSubjs), L),
					LinksSubjs
				),
	if ParentsFacts =/= [] ->
		io:fwrite("Parents' facts: ~W~n", [ParentsFacts,19]);
		true -> empty
	end,
	if ChildrenFacts =/= [] ->
		io:fwrite("Object's and children's facts: ~W~n", [ChildrenFacts,19]);
		true -> empty
	end,
	if AllLinks =/= [] ->
		io:fwrite("Facts with link : ~W~n", [AllLinks,19]);
		true -> empty
	end.
