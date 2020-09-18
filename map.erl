-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

%updates the Map to reflect that Node has directional links to all nodes in the list Links. The old entry is removed.
%keysearch match anykey
%keysearch(london,[2], [{l,[london,paris]}, {london,[london,paris]}]).
% keyreplce (london,[{sim,[simon,stockholm]}], [{london2,[london,paris]}, {london,[london,paris]}]).
%keyreplce match any key where index > 0
update(Node, Links, Map) ->
    R0 = lists:keysearch(Node, 1, Map),
    case R0 of
	{value, {Node, _List}} ->
	    lists:keyreplace(Node, 1, Map, {Node, Links});
	false ->
	    [{Node, Links}|Map]
    end.
%try to find the key map:reachable(london, [{london,[london,paris]}]). --> true
reachable(Node, Map) ->
    R0 = lists:keysearch(Node, 1, Map),
    case R0 of
	{value, {_Node, List}} ->
	    List;
	false ->
	    []
    end.
%[] = Acc0
%returns a list of all nodes in the map
%map:all_nodes([{berlin,[london,paris]}, {stock, [norway, mosko]}]).
%right ti left = [stock,norway,mosko,berlin,london,paris]
%convert cuz some city might not have a tail
all_nodes(Map) ->
    L1 = lists:foldl(fun({Node, Links}, Output) -> [Node|Links] ++ Output end, [], Map),
    Set = sets:from_list(L1),
    sets:to_list(Set).