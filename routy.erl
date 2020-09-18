-module(routy).
-export([start/2, stop/1, init/1, status/1]).
 
 
start(Reg, Name) ->
	register(Reg, spawn(fun() -> init(Name) end)).
 
stop(Node) ->
	Node ! stop,
	unregister(Node).
 
status(Node) ->
	Node ! status.


init(Name) ->
	Intf = intf:new(),
	Map = map:new(),
	Table = dijkstra:table(Intf, Map),
	Hist = hist:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).
 
 
router(Name, N, Hist, Intf, Table, Map) ->
	receive
		{add, Node, Pid} ->
			Ref = erlang:monitor(process,Pid),
			Intf1 = intf:add(Node, Ref, Pid, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
 
		{remove, Node} ->
			{ok, Ref} = intf:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = intf:remove(Node, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
 
		{'DOWN', Ref, process, _, _}  ->
			{ok, Down} = intf:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			Intf1 = intf:remove(Down, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
 
		{links, Node, R, Links} ->
			case hist:update(Node, R, Hist) of
				{new, Hist1} ->
					intf:broadcast({links, Node, R, Links}, Intf),
					Map1 = map:update(Node, Links, Map),
					router(Name, N, Hist1, Intf, Table, Map1);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;
 
		update ->
			Table1 = dijkstra:table(intf:list(Intf), Map),
			router(Name, N, Hist, Intf, Table1, Map);
 
		broadcast ->
			Message = {links, Name, N, intf:list(Intf)},
			intf:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);
			% matches only when the destination is Name (the router itself). This means the message has reached its final destination.
		{route, Name, _From, Message} ->
			io:format("~w: received message ~w ~n", [Name, Message]),
			router(Name, N, Hist, Intf, Table, Map);
		%The second one covers the case when the message has not reached its final destination and needs to be forwarded by the router.
		{route, To, From, Message} ->
			io:format("~w: routing message (~w)", [Name, Message]),
			case dijkstra:route(To, Table) of
			{ok, Gw} ->
				case intf:lookup(Gw, Intf) of
				{ok, Pid} ->
					Pid ! {route, To, From, Message};
				notfound ->
					ok
				end;
			notfound ->
				ok
			end,
			router(Name, N, Hist, Intf, Table, Map);
			
		%send is just a shortcut for sending a message from one router to another. 
		%For example, to send the message "hello" from a router r1 to r2, you could either write either of the following:
		%r1 ! {route, r2, r1, "hello"}
		%r1 ! {send, r2, "hello"}
		{send, To, Message} ->
			self() ! {route, To, Name, Message},
			router(Name, N, Hist, Intf, Table, Map);
		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);

		stop ->
			ok;
		status ->
			io:format("Name: ~w\n N: ~w\nHistory: ~w\nInterfaces: ~w\nTable: ~w\nMap: ~w\n", [Name, N, Hist, Intf, Table, Map]),
			router(Name, N, Hist, Intf, Table, Map)
	end.