%avoid send messages forever-cyclic paths
-module(hist).
-export([new/1, update/3]).

%Return a new history, where messages from Name will
%always be seen as old.
new(Name) ->
    [{Name, inf}].

%Check if message number N from the Node
%is old or new. If it is old then return old but if it new return {new, Updated}
%where Updated is the updated history.
update(Node, N, History) ->
    R1 = lists:keyfind(Node, 1, History),
    case R1 of
	{Node, LatestNr} ->
	    if N > LatestNr ->
		    {new, lists:keyreplace(Node, 1, History, {Node, N})};
	       true ->
		    old
	    end;
	false  ->
	    {new, [{Node, N}|History]}
    end.