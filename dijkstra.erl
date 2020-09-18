-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

%returns the length of the shortest path to the
%node or 0 if the node is not found.
entry(Node, Sorted) ->
    R0 = lists:keysearch(Node, 1, Sorted),
    case R0 of
	{value, {Node, Length, _Gateway}} ->
	    Length;
	false ->
	    0
    end.

%replaces the entry for Node in Sorted with a new entry having a new length N and Gateway
%keysort([{simon, {sss,333}},{bin, {sss,333}},{abin, {sss,333}}],[],[],[]).
%[{abin,{sss,333}},{bin,{sss,333}},{simon,{sss,333}}]
replace(Node, N, Gateway, Sorted) ->
    R0 = lists:keysearch(Node, 1, Sorted),
    case R0 of
	{value, {Node, _OldLength, _OldGateway}} ->
	    L1 = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
	    lists:keysort(2, L1);
	false ->
	    Sorted
    end .

%update the list Sorted given
%the information that Node can be reached in N hops using Gateway.
%If no entry is found then no new entry is added. Only if we have a
%better (shorter) path should we replace the existing entry
update(Node, N, Gateway, Sorted) ->
    L2 = entry(Node, Sorted),
    if N < L2 ->
	    replace(Node, N, Gateway, Sorted);
       true ->
	    Sorted
    end.

%If there are no more entries in the sorted list then we are done and the
%given routing table is complete.
iterate([], _Map, Table) ->
    Table;

%If the first entry is a dummy entry with an infinite path to a city we
%know that the rest of the sorted list is also of infinite length and the
%given routing table is complete.

iterate([{_Node, inf, _Gateway}|_T], _Map, Table) ->
    Table;

%Otherwise, take the first entry in the sorted list, find the nodes in the
%map reachable from this entry and for each of these nodes update the
%Sorted list. The entry that you took from the sorted list is added to
%the routing table.


iterate([{Node, Length, Gateway}|T], Map, Table) ->
    R1 = map:reachable(Node, Map),
    N1 = lists:foldl(fun(N, Sorted) -> update(N, Length + 1, Gateway, Sorted) end, T, R1),
    iterate(N1, Map, [{Node, Gateway}|Table]).

%construct a routing table given the gateways and a map.
%take a list of gateways and a map and produce a routing table with one entry per node in the map. The table could be a list of entries where each
%entry states which gateway to use to find the shortest path to a node (if we have a path).
table(Gateways, Map) ->
    A0 = map:all_nodes(Map),
    L2 = lists:map(fun(Node) -> {Node, inf, unknown} end, A0),
    L3 = lists:foldl(fun(Node, L) -> update(Node, 0, Node, L) end, L2, Gateways),
    iterate(L3, Map, []).


% search the routing table and return the gateway
%suitable to route messages to a node. If a gateway is found we should
%return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
    R0 = lists:keysearch(Node, 1, Table),
    case R0 of
	{value, {Node, Gateway}} ->
	    {ok, Gateway};
	false ->
	    notfound
    end.