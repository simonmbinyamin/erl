-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

%return an empty set of interfaces.
new() ->
    [].

%a process reference and a process identifier
%add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid}|Intf].

%remove an entry given a name of an interface,return a new set of interfaces.
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

%find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
    R1 = lists:keyfind(Name, 1, Intf),
    case R1 of
	{Name, _Ref, Pid} ->
	    {ok, Pid};
	false  ->
	    notfound
    end.

%find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf) ->
    R1 = lists:keyfind(Name, 1, Intf),
    case R1 of
	{Name, Ref, _Pid} ->
	    {ok, Ref};
	false  ->
	    notfound
    end.

%find the name of an entry given a reference andreturn {ok, Name} or notfound.
name(Ref, Intf) ->
    R1 = lists:keyfind(Ref, 2, Intf),
    case R1 of
	{Name, Ref, _Pid} ->
	    {ok, Name};
	false  ->
	    notfound
    end.

%return a list with all names.
list(Intf) ->
    lists:map(fun({Name, _Ref, _Pid}) -> Name end, Intf).

%send the message to all interface processes
broadcast(Message, Intf) ->
    lists:foreach(fun({_Name, _Ref, Pid}) -> Pid ! Message end, Intf).