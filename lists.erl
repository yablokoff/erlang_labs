-module(lab_lists).
-export([len/1]).

% normal recursion
len([]) -> 0.
len([_]) -> 1.
len([_|Tail]) -> 1 + len(Tail).