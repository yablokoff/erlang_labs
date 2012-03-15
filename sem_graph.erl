-module(sem_graph).
-export([findlinks/0]).

%
% Semantic network
% Finds all relations with specified object, subject and type of relationship
%

findlinks()
	->
	{ok,Obj} = io:read("Set object: "),
	Db = [{avtoVAZ, is, auto_producer}, {priora, is, auto}, {priora, produced_by, avtoVAZ}, {priora, presented_in, 2007}, {kalina, is, auto}, {kalina, produced_by, avtoVAZ}, {kalina, presented_in, 2004}, {sheriff_shahov, rides, priora}],
	Objs = [{Object, Link, Subject} || {Object, Link, Subject} <- Db, Object == Obj],
	Subjs = [{Object, Link, Subject} || {Object, Link, Subject} <- Db, Subject == Obj],
	{ok,L} = io:read("Set link: "),
	Links = [{Object, Link, Subject} || {Object, Link, Subject} <- Db, Link == L],
	if Objs =/= [] ->
		io:fwrite("Facts with object : ~W~n", [Objs,9]);
		true -> empty
	end,
	if Subjs =/= [] ->
		io:fwrite("Facts with subject : ~W~n", [Subjs,9]);
		true -> empty
	end,
	if Links =/= [] ->
		io:fwrite("Facts with link : ~W~n", [Links,9]);
		true -> empty
	end
	.
